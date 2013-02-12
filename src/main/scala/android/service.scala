package info.hermesnav.android

import java.io.File

import collection.mutable.ListBuffer
import concurrent._
import ExecutionContext.Implicits.global

import android.app._
import android.content._
import android.location._
import android.os._
import android.provider._
import android.util.Log
import android.widget._
import jsqlite._

import info.hermesnav.core._
import events._
import model.{AndroidMap, AndroidPerspective}
import preferences._

class HermesService extends Service with LocationListener {

  private lazy val locationManager:LocationManager = getSystemService(Context.LOCATION_SERVICE).asInstanceOf[LocationManager]

  def getNotification(msg:String) =
    new Notification.Builder(this)
      .setContentTitle(msg)
      .setSmallIcon(R.drawable.app_icon)
      .setContentIntent(PendingIntent.getActivity(this, 0, new Intent(this, classOf[ui.Hermes]), 0))
      .getNotification

  private lazy val notificationManager = getSystemService(Context.NOTIFICATION_SERVICE).asInstanceOf[NotificationManager]

  private var maps = List[AndroidMap]()

  private def loadMap() {
    val dir = getExternalFilesDir(null)
    dir.mkdir()
    val features = new Database()
    val graph = new Database()
    try {
      val featuresFile = new File(dir, "features.db")
      if(!featuresFile.exists)
        featuresFile.createNewFile()
      features.open(featuresFile.toString, Constants.SQLITE_OPEN_READONLY)
      val graphFile = new File(dir, "graph.db")
      if(!graphFile.exists)
        graphFile.createNewFile()
      graph.open(graphFile.toString, Constants.SQLITE_OPEN_READONLY)
      maps = List(AndroidMap(features, graph))
      sendMessage(getString(R.string.mapLoaded))
    } catch {
      case e:Throwable =>
        Log.d("hermes", "Error opening map", e)
        maps = Nil
    }
  }

  private var initialized = false

  override def onCreate {
    super.onCreate()
    service = Some(this)
    //Thread.setDefaultUncaughtExceptionHandler(new info.thewordnerd.CustomExceptionHandler("/sdcard"))
    Preferences(this)
    Sensors()
    loadMap()
    setLocationEnabled(true)
    startForeground(1, getNotification(getString(R.string.app_name)))
    initialized = true
  }

  override def onDestroy() {
    super.onDestroy()
    service = None
    maps.foreach(_.close())
    setLocationEnabled(false)
    stopForeground(true)
    notificationManager.cancelAll()
    initialized = false
  }

  class LocalBinder extends Binder {
    def getService = HermesService.this
  }

  val binder = new LocalBinder

  override def onBind(intent:Intent):IBinder = binder

  private var locationEnabled = false

  def locationEnabled_? = locationEnabled

  def setLocationEnabled(v:Boolean) {
    v match {
      case true if(locationEnabled) =>
      case true =>
        val criteria = new Criteria
        criteria.setAccuracy(Criteria.ACCURACY_FINE)
        criteria.setBearingAccuracy(Criteria.ACCURACY_HIGH)
        criteria.setHorizontalAccuracy(Criteria.ACCURACY_HIGH)
        criteria.setSpeedAccuracy(Criteria.ACCURACY_HIGH)
        
        val provider = Option(locationManager.getBestProvider(criteria, true)).getOrElse(LocationManager.GPS_PROVIDER)
        locationManager.requestLocationUpdates(provider, 0, 0, this)
      case false if(!locationEnabled) =>
      case false => locationManager.removeUpdates(this)
    }
    locationEnabled = v
  }

  private val providerStatuses:collection.mutable.Map[String, Int] = collection.mutable.Map.empty

  private val handler = new Handler()

  private def sendMessage(msg:String, persistent:Boolean = false) = {
    if(initialized) {
      handler.post {
        Toast.makeText(this, msg, Toast.LENGTH_SHORT).show()
      }
      if(persistent)
        startForeground(1, getNotification(msg))
    }
  }

  private var lastDirection:Option[Direction] = None

  DirectionChanged += { (direction:Option[Direction]) =>
    for(
      dir <- direction;
      lastDir <- lastDirection
    ) {
      if(Preferences.directionAnnouncementPrecision != DirectionAnnouncementPrecision.Off)
        Preferences.directionAnnouncementPrecision match {
          case DirectionAnnouncementPrecision.Low =>
            val ccd = dir.coarseCardinalDirection
            val lcd = lastDir.coarseCardinalDirection
            if(ccd.toString != lcd.toString)
              sendMessage(ccd.toString)
          case DirectionAnnouncementPrecision.High =>
            val ccd = dir.fineCardinalDirection
            val lcd = lastDir.fineCardinalDirection
            if(ccd.toString != lcd.toString)
              sendMessage(ccd.toString)
          case _ =>
        }
    }
  }

  private var lastSpeed:Option[Speed] = None

  private var lastAccuracy:Option[Distance] = None

  AccuracyChanged += { (accuracy:Option[Distance]) =>
    accuracy.foreach { acc =>
      if(Preferences.announceAccuracyChanges_? && accuracy != lastAccuracy)
        sendMessage(acc.to(Preferences.measurementSystem).toString)
    }
  }

  private var lastProvider:Option[String] = None

  private var lastNearestPath:Option[Path] = None

  NearestPathChanged += { path:Option[Path] =>
    path.map { p =>
      lazy val msg = getString(R.string.nearestPath, p.toString)
      if(lastNearestPath == None || p.name == None)
        sendMessage(msg, true)
      else {
        lastNearestPath.foreach { l =>
          if(l.name != p.name)
            sendMessage(msg, true)
        }
        true
      }
    }.getOrElse {
      try {
        sendMessage(getString(R.string.offRoad), true)
      } catch {
        case e => Log.e("hermes", "Error sending message", e)
      }
    }
  }

  private var lastNearestIntersection:Option[IntersectionPosition] = None

  private var pingedIntersections = Map[IntersectionPosition, Long]()

  private var points = List[PointOfInterest]()

  NearestIntersectionChanged += { intersection:Option[IntersectionPosition] =>
    pingedIntersections = pingedIntersections.filter(System.currentTimeMillis-_._2 <= 120000)
    intersection.foreach { i =>
      pingedIntersections.get(i).getOrElse {
        val perspective = i.perspective
        val msg = if(perspective.vehicular) {
          val paths = perspective.nearestPath.map { np =>
            i.pathsExcept(np)
          }.getOrElse(i.paths)
          toSentence(paths.map(_.toString))
        } else
          i.name
        sendMessage(msg)
      }
      pingedIntersections += (i -> System.currentTimeMillis)
    }
  }

  private var previousPerspective:Option[Perspective] = None

  private var compassEnabled = false

  CompassEnabled += { enabled:Boolean =>
    if(enabled && !compassEnabled)
      sendMessage("Compass enabled")
    else if(!enabled && compassEnabled)
      sendMessage("Compass disabled")
    compassEnabled = enabled
  }

  private var processing = false

  private var unprocessedLocation:Option[Location] = None

  def onLocationChanged(loc:Location) {
    val spd = Option(loc.getSpeed).map { s =>
      Speed(Distance(s), second)
    }
    val dir = Option(loc.getBearing).map(Direction(_))
    if(dir != lastDirection)
      DirectionChanged(dir)
    lastDirection = dir
    if(lastSpeed != spd)
      SpeedChanged(spd.map(_ to hours))
    lastSpeed = spd
    val acc = Option(loc.getAccuracy).map(Distance(_))
    if(acc != lastAccuracy)
      AccuracyChanged(acc)
    lastAccuracy = acc
    val provider = Option(loc.getProvider)
    if(provider != lastProvider)
      ProviderChanged(provider)
    lastProvider = provider
    if(processing) {
      unprocessedLocation = Some(loc)
      return
    }
    processing = true
    future {
      Option(Looper.myLooper).getOrElse(Looper.prepare())
      val p = new AndroidPerspective(maps, loc.getLatitude, loc.getLongitude, dir, (loc.getSpeed meters) per second, loc.getTime, previousPerspective)
      PerspectiveChanged(p)
      val np = p.nearestPath
      if(np != lastNearestPath)
        NearestPathChanged(np)
      lastNearestPath = np
      val ni = p.nearestIntersection
      if(ni != lastNearestIntersection)
        NearestIntersectionChanged(ni)
      lastNearestIntersection = ni
      points = p.nearestPoints()
      NearestPoints(points)
      previousPerspective = Some(p)
      processing = false
      unprocessedLocation.foreach { l =>
        unprocessedLocation = None
        onLocationChanged(l)
      }
    }
  }

  def onProviderDisabled(provider:String) {
    providerStatuses -= provider
  }

  def onProviderEnabled(provider:String) {
    providerStatuses(provider) = 0
  }

  def onStatusChanged(provider:String, status:Int, extras:Bundle) {

    def calcMsg = provider+" "+(status match {
      case LocationProvider.OUT_OF_SERVICE => "out of service"
      case LocationProvider.TEMPORARILY_UNAVAILABLE => "unavailable"
      case LocationProvider.AVAILABLE => "available"
    })

    if(providerStatuses.get(provider) == None)
      providerStatuses(provider) = LocationProvider.TEMPORARILY_UNAVAILABLE

    providerStatuses.get(provider) match {
      case Some(s) if(s == status) =>
      case _ => sendMessage(calcMsg)
    }
    providerStatuses(provider) = status
  }

}
