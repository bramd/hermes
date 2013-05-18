package info.hermesnav.android
package ui

import android.app._
import android.content._
import android.os._
import android.preference._
import android.view._
import android.widget._
import org.scaloid.common.{LocalServiceConnection, SActivity}

import info.hermesnav.core._
import events._
import preferences._
import services._

class Hermes extends SActivity {

  private var svc = new LocalServiceConnection[LocationService]

  private val updateNearestPath = { np:Option[Path] =>
    val v = findViewById(R.id.nearestPath).asInstanceOf[TextView]
    runOnUiThread {
      v.setText(np.map { p =>
        getString(R.string.nearestPath, p.toString)
      }.getOrElse(getString(R.string.offRoad)))
    }
  }

  private val updateNearestIntersection = {p:Perspective =>
    val v = findViewById(R.id.nearestIntersection).asInstanceOf[TextView]
    runOnUiThread {
      v.setText(p.nearestIntersection.map { i =>
        i.name+": "+p.distanceTo(i).to(Preferences.measurementSystem)+p.bearingTo(i).map(" "+_).getOrElse("")
      }.getOrElse(""))
    }
  }

  private val updateDirection = { dir:Option[Direction] =>
    val v = findViewById(R.id.direction).asInstanceOf[TextView]
    runOnUiThread {
      v.setText(dir.map { d =>
        d.toString
      }.getOrElse(""))
    }
  }

  private val updateSpeed = { spd:Option[Speed] =>
    val v = findViewById(R.id.speed).asInstanceOf[TextView]
    runOnUiThread {
      v.setText(spd.map { s =>
        s.to(Preferences.measurementSystem).toString
      }.getOrElse(""))
    }
  }

  private val updateAccuracy = { acc:Option[Distance] =>
    val v = findViewById(R.id.accuracy).asInstanceOf[TextView]
    runOnUiThread {
      v.setText(acc.map { a =>
        a.to(Preferences.measurementSystem).toString
      }.getOrElse(""))
    }
  }

  private val updateProvider = { p:Option[String] =>
    val v = findViewById(R.id.provider).asInstanceOf[TextView]
    runOnUiThread {
      v.setText(p.getOrElse(""))
    }
  }

  private val updateNearestPoints = { points:List[PointOfInterest] =>
    val list = findViewById(R.id.points).asInstanceOf[ListView]
    val adapter = new ArrayAdapter[PointOfInterest](this, android.R.layout.simple_list_item_1, points.toArray)
    runOnUiThread {
      list.setAdapter(adapter)
    }
  }

  onCreate {
    setContentView(R.layout.info)
    val serviceIntent = new Intent(this, classOf[LocationService])
    startService(serviceIntent)
  }

  onDestroy {
    NearestPathChanged -= updateNearestPath
    PerspectiveChanged -= updateNearestIntersection
    DirectionChanged -= updateDirection
    SpeedChanged -= updateSpeed
    AccuracyChanged -= updateAccuracy
    ProviderChanged -= updateProvider
    NearestPoints -= updateNearestPoints
  }

  svc.onConnected {
    NearestPathChanged += updateNearestPath
    PerspectiveChanged += updateNearestIntersection
    DirectionChanged += updateDirection
    SpeedChanged += updateSpeed
    AccuracyChanged += updateAccuracy
    ProviderChanged += updateProvider
    NearestPoints += updateNearestPoints
    println("Did it.")
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
    svc.service.stopSelf()
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
