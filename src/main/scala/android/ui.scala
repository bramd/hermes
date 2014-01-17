package info.hermesnav.android
package ui

import android.app._
import android.content._
import android.os._
import android.preference._
import android.view._
import android.widget._
import org.scaloid.common.{info => _, Preferences => _, SArrayAdapter => _, _}
import collection.mutable.ListBuffer
import collection.JavaConversions._

import info.hermesnav.core._
import events._
import preferences._
import services._

class PointOfInterestArrayAdapter[P<:PointOfInterest](context:Context, resource:Int, objects:ListBuffer[PointOfInterest]) extends ArrayAdapter[PointOfInterest](context, resource, objects) {
  override def hasStableIds = true

  override def getItemId(pos:Int) = this.getItem(pos).identifier.identifier
}

class Hermes extends SActivity {

  private lazy val nearestPointsAdapter:PointOfInterestArrayAdapter[PointOfInterest] = new PointOfInterestArrayAdapter(this, android.R.layout.simple_list_item_1, new ListBuffer[PointOfInterest]())
  private lazy val nearestPath = new STextView

  private val updateNearestPath = { np:Option[Path] =>
    runOnUiThread {
      nearestPath.text = np.map { p =>
        getString(R.string.nearestPath, p.toString)
      }.getOrElse(getString(R.string.offRoad))
    }
  }

  private lazy val nearestIntersection = new STextView

  private val updateNearestIntersection = {p:Perspective =>
    runOnUiThread {
      nearestIntersection.text = p.nearestIntersection.map { i =>
        i.name+": "+p.distanceTo(i).to(Preferences.measurementSystem)+p.bearingTo(i).map(" "+_).getOrElse("")
      }.getOrElse("")
    }
  }

  private lazy val direction = new STextView

  private val updateDirection = { dir:Option[Direction] =>
    runOnUiThread {
      direction.text = dir.map { d =>
        d.toString
      }.getOrElse("")
    }
  }

  private lazy val speed = new STextView

  private val updateSpeed = { spd:Option[Speed] =>
    runOnUiThread {
      speed.text = spd.map { s =>
        s.to(Preferences.measurementSystem).toString
      }.getOrElse("")
    }
  }

  private lazy val altitude = new STextView

  private val updateAltitude = { alt:Option[Distance] =>
    runOnUiThread {
      altitude.text = alt.map { a =>
        a.to(Preferences.measurementSystem).toString
      }.getOrElse("")
    }
  }

  private lazy val accuracy = new STextView

  private val updateAccuracy = { acc:Option[Distance] =>
    runOnUiThread {
      accuracy.text = acc.map { a =>
        a.to(Preferences.measurementSystem).toString
      }.getOrElse("")
    }
  }

  private lazy val provider = new STextView

  private val updateProvider = { p:Option[String] =>
    runOnUiThread {
      provider.text = p.getOrElse("")
    }
  }

  private lazy val nearestPoints = new SListView {
    /*onScroll { (l:ListView, firstVisible:Int, visibleCount:Int, totalCount:Int) =>
    }*/
  }

  private val updateNearestPoints = { points:List[PointOfInterest] =>
    runOnUiThread {
      nearestPointsAdapter.clear()
      nearestPointsAdapter.addAll(points)
    }
  }

  private val locationService = new LocalServiceConnection[LocationService] {
    onConnected {
      NearestPathChanged += updateNearestPath
      PerspectiveChanged += updateNearestIntersection
      DirectionChanged += updateDirection
      SpeedChanged += updateSpeed
      AltitudeChanged += updateAltitude
      AccuracyChanged += updateAccuracy
      ProviderChanged += updateProvider
      NearestPoints += updateNearestPoints
      println("Did it.")
    }
  }

  onCreate {
    nearestPoints.setAdapter(nearestPointsAdapter)
    contentView = new SVerticalLayout {
      style {
        case t:STextView => t.focusable = true
      }
      this += nearestPath
      this += nearestIntersection
      this += nearestPoints
      this += new SLinearLayout {
        style {
          case v if(v == orientation) =>
            v.gravity = Gravity.LEFT
            v
          case v if(v == provider) =>
            v.gravity = Gravity.RIGHT
            v.weight = 1
            v
          case v =>
            v.gravity = Gravity.CENTER_HORIZONTAL
            v.weight = 1
            v
        }
        orientation = HORIZONTAL
        this += direction
        this += speed
        this += altitude
        this += accuracy
        this += provider
      }
    }
    startService[LocationService]
    startService[ActivityDetectorConnector]
    LocationService.activelyViewing = true
  }

  onPause {
    LocationService.activelyViewing = false
  }

  onResume {
    LocationService.activelyViewing = true
    startService[LocationService]
  }

  onDestroy {
    NearestPathChanged -= updateNearestPath
    PerspectiveChanged -= updateNearestIntersection
    DirectionChanged -= updateDirection
    SpeedChanged -= updateSpeed
    AltitudeChanged -= updateAltitude
    AccuracyChanged -= updateAccuracy
    ProviderChanged -= updateProvider
    NearestPoints -= updateNearestPoints
    LocationService.activelyViewing = false
  }

  private var menu:Option[Menu] = None

  override def onCreateOptionsMenu(m:Menu):Boolean = {
    menu = Some(m)
    val inflater = new MenuInflater(this)
    inflater.inflate(R.menu.main, m)
    true
  }

  override def onOptionsItemSelected(item:MenuItem) = {
    item.getItemId match {
      case R.id.settings => startActivity(new Intent(this, classOf[Settings]))
      case R.id.exit => exit()
    }
    true
  }

  def exit() {
    stopService[LocationService]
    if(!Preferences.activateWhenMoving_?)
      stopService[ActivityDetectorConnector]
    finish()
  }

}

class StockPreferenceFragment extends PreferenceFragment {
  override def onCreate(bundle:Bundle) {
    super.onCreate(bundle)
    val res = getActivity.getResources.getIdentifier(getArguments.getString("resource"), "xml", getActivity.getPackageName)
    addPreferencesFromResource(res)
  }
}

class Settings extends PreferenceActivity {
  override def onBuildHeaders(target:java.util.List[PreferenceActivity.Header]) {
    loadHeadersFromResource(R.xml.preference_headers, target)
  }
}
