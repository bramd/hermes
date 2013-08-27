package info.hermesnav.android
package receivers

import android.content._

import preferences._

class BootReceiver extends BroadcastReceiver {
  def onReceive(c:Context, i:Intent) {
    Preferences(c)
    if(Preferences.activateWhenMoving_?)
      c.startService(new Intent(c, classOf[services.ActivityDetectorConnector]))
  }
}
