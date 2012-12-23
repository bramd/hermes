package info.hermesnav.android
package ui

import android.app._
import android.content._
import android.os._
import android.preference._
import android.view._
import android.widget._

import info.hermesnav.core._
import preferences._

class Hermes extends Activity with ServiceConnection {

  private var svc:Option[HermesService] = None

  private def updateNearestPath(np:Option[Path]) {
    val v = findViewById(R.id.nearestPath).asInstanceOf[TextView]
    runOnUiThread {
      v.setText(np.map { p =>
        getString(R.string.nearestPath, p.toString)
      }.getOrElse(getString(R.string.offRoad)))
    }
  }

  private def updateNearestIntersection(p:Perspective) {
    val v = findViewById(R.id.nearestIntersection).asInstanceOf[TextView]
    runOnUiThread {
      v.setText(p.nearestIntersection.map { i =>
        i.name+": "+p.distanceTo(i).to(Preferences.measurementSystem)+p.bearingTo(i).map(" "+_).getOrElse("")
      }.getOrElse(""))
    }
  }

  private def updateDirection(dir:Option[Direction]) {
    val v = findViewById(R.id.direction).asInstanceOf[TextView]
    runOnUiThread {
      v.setText(dir.map { d =>
        d.toString
      }.getOrElse(""))
    }
  }

  private def updateSpeed(spd:Option[Speed]) {
    val v = findViewById(R.id.speed).asInstanceOf[TextView]
    runOnUiThread {
      v.setText(spd.map { s =>
        s.to(Preferences.measurementSystem).toString
      }.getOrElse(""))
    }
  }

  private def updateAccuracy(acc:Option[Distance]) {
    val v = findViewById(R.id.accuracy).asInstanceOf[TextView]
    runOnUiThread {
      v.setText(acc.map { a =>
        a.to(Preferences.measurementSystem).toString
      }.getOrElse(""))
    }
  }

  private def updateProvider(p:Option[String]) {
    val v = findViewById(R.id.provider).asInstanceOf[TextView]
    runOnUiThread {
      v.setText(p.getOrElse(""))
    }
  }

  private def updateNearestPoints(points:List[PointOfInterest]) {
    val list = findViewById(R.id.points).asInstanceOf[ListView]
    val adapter = new ArrayAdapter[PointOfInterest](this, android.R.layout.simple_list_item_1, points.toArray)
    runOnUiThread {
      list.setAdapter(adapter)
    }
  }

  override def onCreate(bundle:Bundle) {
    super.onCreate(bundle)
    setContentView(R.layout.info)
    val serviceIntent = new Intent(this, classOf[HermesService])
    startService(serviceIntent)
    bindService(serviceIntent, this, Context.BIND_AUTO_CREATE)
  }

  override def onDestroy() {
    super.onDestroy()
    svc.foreach { s =>
      s.removeNearestPathChangedHandler(updateNearestPath)
      s.removePerspectiveChangedHandler(updateNearestIntersection)
      s.removeDirectionChangedHandler(updateDirection)
      s.removeSpeedChangedHandler(updateSpeed)
      s.removeAccuracyChangedHandler(updateAccuracy)
      s.removeProviderChangedHandler(updateProvider)
      s.removeNearestPointsHandler(updateNearestPoints)
    }
    unbindService(this)
  }

  def onServiceConnected(className:ComponentName, rawBinder:IBinder) {
    val s = rawBinder.asInstanceOf[HermesService#LocalBinder].getService
    svc = Some(s)
    s.onNearestPathChanged(updateNearestPath)
    s.onPerspectiveChanged(updateNearestIntersection)
    s.onDirectionChanged(updateDirection)
    s.onSpeedChanged(updateSpeed)
    s.onAccuracyChanged(updateAccuracy)
    s.onProviderChanged(updateProvider)
    s.onNearestPoints(updateNearestPoints)
  }

  def onServiceDisconnected(className:ComponentName) {
    svc = None
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
    svc.foreach(_.stopSelf)
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
