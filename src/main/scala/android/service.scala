package info.hermesnav.android

import java.io.File

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
import model.AndroidPerspective

class HermesService extends Service {

  private lazy val locationManager:LocationManager = getSystemService(Context.LOCATION_SERVICE).asInstanceOf[LocationManager]

  def getNotification(msg:String) =
    new Notification.Builder(this)
      .setContentTitle(msg)
      .setSmallIcon(R.drawable.app_icon)
      .setContentIntent(PendingIntent.getActivity(this, 0, new Intent(this, classOf[ui.Hermes]), 0))
      .getNotification

  private lazy val notificationManager = getSystemService(Context.NOTIFICATION_SERVICE).asInstanceOf[NotificationManager]

  private lazy val coarseUpdater = new LocationUpdater(Some(coarseCriteria))

  private lazy val fineUpdater = new LocationUpdater(None, coarseUpdater :: Nil)

  private var database:Option[Database] = None

  private def loadDatabase() {
    val dir = getExternalFilesDir(null)
    dir.mkdir()
    val db = new Database()
    try {
      val dbFile = new File(dir, "map.db")
      if(!dbFile.exists)
        dbFile.createNewFile()
      db.open(dbFile.toString, Constants.SQLITE_OPEN_READWRITE)
      database = Some(db)
      sendMessage(getString(R.string.mapLoaded))
    } catch {
      case e =>
        Log.d("hermes", "Error opening database", e)
        database = None
    }
  }

  override def onCreate {
    super.onCreate
    Preferences(this)
    loadDatabase()
    setLocationEnabled(true)
    startForeground(1, getNotification(getString(R.string.app_name)))
  }

  override def onDestroy() {
    super.onDestroy()
    database.foreach(_.close())
    setLocationEnabled(false)
    notificationManager.cancelAll
  }

  class LocalBinder extends Binder {
    def getService = HermesService.this
  }

  val binder = new LocalBinder

  override def onBind(intent:Intent):IBinder = binder

  private val coarseCriteria = new Criteria
  coarseCriteria.setAccuracy(Criteria.ACCURACY_COARSE)

  private var locationEnabled = false

  def locationEnabled_? = locationEnabled

  def setLocationEnabled(v:Boolean) {
    v match {
      case true if(locationEnabled) =>
      case true =>
        coarseUpdater.enable
        fineUpdater.enable
      case false if(!locationEnabled) =>
      case false =>
        coarseUpdater.disable
        fineUpdater.disable
    }
    locationEnabled = v
  }

  private val providerStatuses:collection.mutable.Map[String, Int] = collection.mutable.Map.empty

  private def sendMessage(msg:String) {
    Toast.makeText(this, msg, Toast.LENGTH_LONG).show()
    startForeground(1, getNotification(msg))
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
    path.foreach { p =>
      sendMessage(getString(R.string.nearestPath, p.name))
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

  onNearestIntersectionChanged { intersection =>
    pingedIntersections = pingedIntersections.filter(System.currentTimeMillis-_._2 <= 30000)
    intersection.foreach { i =>
      pingedIntersections.get(i).getOrElse {
        sendMessage(i.name)
      }
      pingedIntersections += (i -> System.currentTimeMillis)
    }
  }

  private var previousPerspective:Option[Perspective] = None

  class LocationUpdater(criteria:Option[Criteria] = None, val preferredTo:List[LocationUpdater] = Nil) extends LocationListener {

    private var enabled = false

    def disable() {
      if(!enabled) return
      locationManager.removeUpdates(this)
      enabled = false
    }

    def enable() {
      if(enabled) return
      val provider = criteria.map(locationManager.getBestProvider(_, true)).getOrElse(LocationManager.GPS_PROVIDER)
      locationManager.requestLocationUpdates(provider, 1000, 1f, this)
      enabled = true
    }

    private var processing = false

    def onLocationChanged(loc:Location) {
      val spd = Option(loc.getSpeed).map { s =>
        Speed(Distance(s), second)
      }
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
      val p = Option(loc.getProvider)
      if(p != lastProvider)
        providerChanged(p)
      lastProvider = p
      if(processing)
        return
      processing = true
      database.map { db =>
        val p = new AndroidPerspective(List(db), loc.getLatitude, loc.getLongitude, dir, (loc.getSpeed meters) per second, loc.getTime, previousPerspective)
        val np = p.nearestPath
        if(np != lastNearestPath)
          nearestPathChanged(np)
        lastNearestPath = np
        val ni = p.nearestIntersection
        if(ni != lastNearestIntersection)
          nearestIntersectionChanged(ni)
        lastNearestIntersection = ni
        previousPerspective = Some(p)
      }
      processing = false
    }

    def onProviderDisabled(provider:String) {
      providerStatuses -= provider
      preferredTo.foreach(_.enable())
    }

    def onProviderEnabled(provider:String) {
      providerStatuses(provider) = 0
      preferredTo.foreach(_.disable())
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
        case Some(s) if(status == LocationProvider.AVAILABLE) =>
          preferredTo.foreach (_.disable)
          sendMessage(calcMsg)
        case _ =>
          sendMessage(calcMsg)
          preferredTo.foreach(_.enable)
      }
      providerStatuses(provider) = status
    }

  }

}
