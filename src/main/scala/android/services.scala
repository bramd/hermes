package info.hermesnav.android
package services

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
import org.scaloid.common.{info => _, Preferences => _, _}
import org.mapsforge.map.reader.MapDatabase

import info.hermesnav.core._
import info.hermesnav.mapsforge.MapsforgePerspective
import events._
import preferences._

class LocationService extends LocalService with LocationListener {

  private lazy val locationManager:LocationManager = getSystemService(Context.LOCATION_SERVICE).asInstanceOf[LocationManager]

  def getNotification(msg:String) =
    new Notification.Builder(this)
      .setContentTitle(msg)
      .setSmallIcon(R.drawable.app_icon)
      .setContentIntent(PendingIntent.getActivity(this, 0, new Intent(this, classOf[ui.Hermes]), 0))
      .getNotification

  private lazy val notificationManager = getSystemService(Context.NOTIFICATION_SERVICE).asInstanceOf[NotificationManager]

  private var maps = List[MapDatabase]()

  private def loadMap() {
    val dir = getExternalFilesDir(null)
    dir.mkdir()
    val features = new MapDatabase()
    try {
      val featuresFile = new File(dir, "features.map")
      features.openFile(featuresFile)
      Log.d("hermes", "Opened features map " + dir)
      maps = List(features)
    } catch {
      case e:Throwable =>
        Log.d("hermes", "Error opening map", e)
        maps = Nil
    }
  }

  private var initialized = false

  onCreate {
    service = Some(this)
    //Thread.setDefaultUncaughtExceptionHandler(new info.thewordnerd.CustomExceptionHandler("/sdcard"))
    Preferences(this)
    //Sensors()
    loadMap()
    setLocationEnabled(true)
    startForeground(1, getNotification(getString(R.string.app_name)))
    initialized = true
    Log.d("hermes", "LocationService initialized")
    queuedMessages.reverse.foreach { i =>
      sendMessage(i._1, i._2)
    }
    queuedMessages = Nil
  }

  onDestroy {
    service = None
    setLocationEnabled(false)
    stopForeground(true)
    notificationManager.cancelAll()
    NearestPathChanged.clear
    NearestPoints.clear
    NearestIntersectionChanged.clear
    initialized = false
    stopService
    Log.d("hermes", "LocationService stopped")
  }

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

  private var queuedMessages:List[Tuple2[String, Boolean]] = Nil

  private def sendMessage(msg:String, persistent:Boolean = false) = {
    if(initialized) {
      toast(msg)
      if(persistent)
        startForeground(1, getNotification(msg))
    } else
      queuedMessages ::= (msg, persistent)
  }

  private var lastDirection:Option[Direction] = None

  private var lastCompassAnnouncementTime = 0l

  private var lastAnnouncedDirection = ""

  DirectionChanged += { (direction:Option[Direction]) =>
    direction.foreach { dir =>
      if(Preferences.directionAnnouncementPrecision != DirectionAnnouncementPrecision.Off) {
        val announcement = Preferences.directionAnnouncementPrecision match {
          case DirectionAnnouncementPrecision.Low => dir.coarseCardinalDirection.toString
          case DirectionAnnouncementPrecision.High => dir.fineCardinalDirection.toString
        }
        if(announcement != lastAnnouncedDirection) {
          if(!compassEnabled || (compassEnabled && System.currentTimeMillis-lastCompassAnnouncementTime >= 500)) {
            sendMessage(announcement)
            lastAnnouncedDirection = announcement
            if(compassEnabled)
              lastCompassAnnouncementTime = System.currentTimeMillis
          }
        }
      }
    }
    lastDirection = direction
  }

  private var lastSpeed:Option[Speed] = None

  private var lastAccuracy:Option[Distance] = None

  AccuracyChanged += { (accuracy:Option[Distance]) =>
    accuracy.foreach { acc =>
      if(Preferences.announceAccuracyChanges_? && accuracy != lastAccuracy)
        sendMessage(acc.to(Preferences.measurementSystem).toString)
    }
  }

  private var lastAltitude:Option[Distance] = None

  private var lastProvider:Option[String] = None

  private var lastNearestPath:Option[Path] = None

  NearestPathChanged += { path:Option[Path] =>
    path.map { p =>
      val msg = getString(R.string.nearestPath, p.toString)
      sendMessage(msg, true)
      true
    }.getOrElse {
      try {
        sendMessage(getString(R.string.offRoad), true)
      } catch {
        case e:NullPointerException => // TODO: Fix me
      }
    }
  }

  private var lastNearestIntersection:Option[IntersectionPosition] = None

  private var pingedIntersections = Map[IntersectionPosition, Long]()

  private var points = List[PointOfInterest]()

  NearestIntersectionChanged += { intersection:Option[IntersectionPosition] =>
    Log.d("hermes", "NearestIntersectionChanged")
    pingedIntersections = pingedIntersections.filter(System.currentTimeMillis-_._2 <= 120000)
    intersection.foreach { i =>
      pingedIntersections.get(i).getOrElse {
        val perspective = i.perspective
        val msg = if(perspective.vehicular) {
          val paths = perspective.nearestPath.map { np =>
            i.pathsExcept(np)
          }.getOrElse(i.paths)
          toSentence(paths.toList.map(_.toString))
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

  private var firstRun = true

  def onLocationChanged(loc:Location) {
    val spd = Option(loc.getSpeed).map { s =>
      Speed(Distance(s), second)
    }
    val dir = if(compassEnabled)
      lastDirection
    else {
      val d2 = spd.filter(_.distance.units == 0).flatMap { s =>
        previousPerspective.map(_.direction)
      }.getOrElse(Option(loc.getBearing).map(Direction(_)))
      if(d2 != lastDirection)
        DirectionChanged(d2)
      lastDirection = d2
      d2
    }
    if(firstRun || lastSpeed != spd)
      SpeedChanged(spd.map(_ to hours))
    lastSpeed = spd
    val acc = Option(loc.getAccuracy).map(Distance(_))
    if(firstRun || acc != lastAccuracy)
      AccuracyChanged(acc)
    lastAccuracy = acc
    val altitude = Option(loc.getAltitude).map(Distance(_))
    if(firstRun || altitude != lastAltitude)
      AltitudeChanged(altitude)
    lastAltitude = altitude
    val provider = Option(loc.getProvider)
    if(firstRun || provider != lastProvider)
      ProviderChanged(provider)
    lastProvider = provider
    if(processing) {
//      Log.d("hermes", "Still processing...")
      unprocessedLocation = Some(loc)
      return
    }
    processing = true
    Future {
      Log.d("hermes", "newPerspective")
      val p = new MapsforgePerspective(maps, loc.getLatitude, loc.getLongitude, dir, (loc.getSpeed meters) per second, loc.getTime, previousPerspective)
      Log.d("hermes", "PerspectiveChanged")
      PerspectiveChanged(p)
      val np = p.nearestPath
      Log.d("hermescheck", "np: "+np.map(_.classification)+", last: "+lastNearestPath.map(_.classification))
      if(firstRun || np != lastNearestPath) {
        Log.d("hermescheck", "Updating nearest path")
        Log.d("hermes", "NearestPathChanged")
        NearestPathChanged(np)
      }
      lastNearestPath = np
      val ni = p.nearestIntersection
      if(firstRun || ni != lastNearestIntersection) {
        Log.d("hermes", "IntersectionChanged")
        NearestIntersectionChanged(ni)
      }
      lastNearestIntersection = ni
      points = p.nearestPoints(filter=Preferences.hideUnnamedPoints_? match {
        case true => {p => p.name != "Unnamed" }
        case _ => {p => true}
      })
      NearestPoints(points)
      previousPerspective = Some(p)
      processing = false
      firstRun = false
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

object LocationService {
  var activelyViewing = false
}
import com.google.android.gms._
import location._
import DetectedActivity._
import com.google.android.gms.common._
import GooglePlayServicesClient._

class ActivityDetectorConnector extends LocalService with ConnectionCallbacks with OnConnectionFailedListener {

  private lazy val client = new ActivityRecognitionClient(this, this, this)

  onCreate {
    if(GooglePlayServicesUtil.isGooglePlayServicesAvailable(this) == ConnectionResult.SUCCESS)
      client.connect()
  }

  def onConnected(b:Bundle) {
    client.requestActivityUpdates(10000, pendingService(new Intent(this, classOf[ActivityDetector])))
    client.disconnect()
  }

  def onDisconnected() {
  }

  def onConnectionFailed(r:ConnectionResult) {
  }

}

class ActivityDetector extends IntentService("ActivityDetector") {

  import ActivityDetector._

  override protected def onHandleIntent(i:Intent) {
    Preferences(this)
    if(ActivityRecognitionResult.hasResult(i)) {
      val result = ActivityRecognitionResult.extractResult(i)
      val activity = result.getMostProbableActivity
      val t = activity.getType
      t match {
        case STILL =>
          stoppedAt.map { s =>
            if(Preferences.activateWhenMoving_? && !LocationService.activelyViewing && System.currentTimeMillis-s >= 180000) {
              stopService(new Intent(this, classOf[LocationService]))
              stoppedAt = None
              startedAt = None
            }
          }.getOrElse {
            stoppedAt = Some(System.currentTimeMillis)
            startedAt = None
          }
        case TILTING | UNKNOWN =>
        case _ =>
          startedAt.map { s =>
            if(Preferences.activateWhenMoving_? && (t != ON_FOOT || System.currentTimeMillis-s >= 30000)) {
              startService(new Intent(this, classOf[LocationService]))
              stoppedAt = None
              startedAt = None
            }
          }.getOrElse {
            startedAt = Some(System.currentTimeMillis)
            stoppedAt = None
          }
      }
    }
  }

}

object ActivityDetector {

  private var stoppedAt:Option[Long] = None

  private var startedAt:Option[Long] = None

}
