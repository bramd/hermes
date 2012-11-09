package info.hermesnav.android

import java.io.File

import actors.Actor.actor
import collection.mutable.ListBuffer

import android.app._
import android.content._
import android.location._
import android.os._
import android.provider._
import android.util.Log
import android.widget._
import jsqlite._

import info.hermesnav.core._
import model.{AndroidMap, AndroidPerspective}

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
      case e =>
        Log.d("hermes", "Error opening map", e)
        maps = Nil
    }
  }

  private var initialized = false

  override def onCreate {
    super.onCreate()
    Thread.setDefaultUncaughtExceptionHandler(new info.thewordnerd.CustomExceptionHandler("/sdcard"))
    Preferences(this)
    loadMap()
    setLocationEnabled(true)
    startForeground(1, getNotification(getString(R.string.app_name)))
    initialized = true
  }

  override def onDestroy() {
    super.onDestroy()
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
        locationManager.requestLocationUpdates(provider, 1000, 1f, this)
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

  private val directionChangedHandlers = ListBuffer[(Option[Direction]) => Unit]()

  def onDirectionChanged(f:(Option[Direction]) => Unit) = {
    f(lastDirection)
    directionChangedHandlers += f
  }

  def directionChanged(v:Option[Direction]) = directionChangedHandlers.foreach(_(v))

  def removeDirectionChangedHandler(f:(Option[Direction]) => Unit) = directionChangedHandlers -= f

  private var lastSpeed:Option[Speed] = None

  private val speedChangedHandlers = ListBuffer[(Option[Speed]) => Unit]()

  def onSpeedChanged(f:(Option[Speed]) => Unit) = {
    f(lastSpeed)
    speedChangedHandlers += f
  }

  def speedChanged(v:Option[Speed]) = speedChangedHandlers.foreach(_(v))

  def removeSpeedChangedHandler(f:(Option[Speed]) => Unit) = speedChangedHandlers -= f

  private var lastAccuracy:Option[Distance] = None

  private val accuracyChangedHandlers = ListBuffer[(Option[Distance]) => Unit]()

  def onAccuracyChanged(f:(Option[Distance]) => Unit) = {
    f(lastAccuracy)
    accuracyChangedHandlers += f
  }

  def accuracyChanged(a:Option[Distance]) = accuracyChangedHandlers.foreach(_(a))

  def removeAccuracyChangedHandler(f:(Option[Distance]) => Unit) = accuracyChangedHandlers -= f

  private var lastProvider:Option[String] = None

  private val providerChangedHandlers = ListBuffer[(Option[String]) => Unit]()

  def onProviderChanged(f:(Option[String]) => Unit) = {
    f(lastProvider)
    providerChangedHandlers += f
  }

  def providerChanged(v:Option[String]) = providerChangedHandlers.foreach(_(v))

  def removeProviderChangedHandler(f:(Option[String]) => Unit) = providerChangedHandlers -= f

  private var lastNearestPath:Option[Path] = None

  private val nearestPathChangedHandlers = ListBuffer[(Option[Path]) => Unit]()

  def onNearestPathChanged(f:(Option[Path]) => Unit) = {
    f(lastNearestPath)
    nearestPathChangedHandlers += f
  }

  def nearestPathChanged(v:Option[Path]) = nearestPathChangedHandlers.foreach(_(v))

  def removeNearestPathChangedHandler(f:(Option[Path]) => Unit) = nearestPathChangedHandlers -= f

  onNearestPathChanged { path =>
    path.map { p =>
      lazy val msg = getString(R.string.nearestPath, p.name.getOrElse(p.toString))
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
      sendMessage("Off-road", true)
    }
  }

  private var lastNearestIntersection:Option[IntersectionPosition] = None

  private val nearestIntersectionChangedHandlers = ListBuffer[(Option[IntersectionPosition]) => Unit]()

  def onNearestIntersectionChanged(f:(Option[IntersectionPosition]) => Unit) = {
    f(lastNearestIntersection)
    nearestIntersectionChangedHandlers += f
  }

  def nearestIntersectionChanged(v:Option[IntersectionPosition]) = nearestIntersectionChangedHandlers.foreach(_(v))

  def removeNearestIntersectionChangedHandler(f:(Option[IntersectionPosition]) => Unit) = nearestIntersectionChangedHandlers -= f

  private var pingedIntersections = Map[IntersectionPosition, Long]()

  private var points = List[PointOfInterest]()

  private val nearestPointsHandlers = ListBuffer[(List[PointOfInterest]) => Unit]()

  def onNearestPoints(f:(List[PointOfInterest]) => Unit) = {
    f(points)
    nearestPointsHandlers += f
  }

  def nearestPoints(v:List[PointOfInterest]) = nearestPointsHandlers.foreach(_(v))

  def removeNearestPointsHandler(f:(List[PointOfInterest]) => Unit) = nearestPointsHandlers -= f

  onNearestIntersectionChanged { intersection =>
    pingedIntersections = pingedIntersections.filter(System.currentTimeMillis-_._2 <= 30000)
    intersection.foreach { i =>
      pingedIntersections.get(i).getOrElse {
        val perspective = i.perspective
        if(perspective.vehicular) {
          val perspective = i.perspective
          val paths = perspective.nearestPath.map { np =>
            i.pathsExcept(np)
          }.getOrElse(i.paths)
          sendMessage(toSentence(paths.map(_.toString)))
        } else
          sendMessage(i.name)
      }
      pingedIntersections += (i -> System.currentTimeMillis)
    }
  }

  private var previousPerspective:Option[Perspective] = None

  private var processing = false

  private var unprocessedLocation:Option[Location] = None

  def onLocationChanged(loc:Location) {
    val spd = Option(loc.getSpeed).map { s =>
      Speed(Distance(s), second)
    }
    val dir = spd.flatMap { s =>
      if(s.distance.units == 0)
        lastDirection
      else None
    }.orElse(Option(loc.getBearing).map(Direction(_)))
    if(!processing) {
      if(lastSpeed != spd)
        speedChanged(spd.map(_ to hours))
      lastSpeed = spd.map(_ to hours)
      val dir = spd.flatMap { s =>
        if(s.distance.units == 0)
          lastDirection
        else None
      }.orElse(Option(loc.getBearing).map(Direction(_)))
      if(dir != lastDirection)
        directionChanged(dir)
      lastDirection = dir
      val acc = Option(loc.getAccuracy).map(Distance(_))
      if(acc != lastAccuracy)
        accuracyChanged(acc)
      lastAccuracy = acc
      val provider = Option(loc.getProvider)
      if(provider != lastProvider)
        providerChanged(provider)
      lastProvider = provider
    } else {
      unprocessedLocation = Some(loc)
      return
    }
    processing = true
    actor {
      Option(Looper.myLooper).getOrElse(Looper.prepare())
      val p = new AndroidPerspective(maps, loc.getLatitude, loc.getLongitude, dir, (loc.getSpeed meters) per second, loc.getTime, previousPerspective)
      val np = p.nearestPath
      val nearestPath = System.currentTimeMillis
      if(np != lastNearestPath)
        nearestPathChanged(np)
      lastNearestPath = np
      val ni = p.nearestIntersection
      if(ni != lastNearestIntersection)
        nearestIntersectionChanged(ni)
      lastNearestIntersection = ni
      points = p.nearestPoints()
      nearestPoints(points)
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
