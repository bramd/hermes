package info.hermesnav.android

import android.content.{Context, SharedPreferences}
import android.net.Uri
import android.preference.PreferenceManager
import android.provider.Settings

object Preferences {

  private var preferences:SharedPreferences = null

  private var context:Context = null

  def apply(c:Context) {
    context = c
    preferences = PreferenceManager.getDefaultSharedPreferences(context)
  }

  def intersectionNotificationTone = preferences.getString("intersection_notification_tone", "") match {
    case "" => Settings.System.DEFAULT_NOTIFICATION_URI
    case v => Uri.parse(v)
  }

  def pointNotificationTone = preferences.getString("point_notification_tone", "") match {
    case "" => Settings.System.DEFAULT_NOTIFICATION_URI
    case v => Uri.parse(v)
  }

  def pingNearest = preferences.getBoolean("ping_nearest", false)

  def pingNearest_=(v:Boolean) {
    val editor = preferences.edit()
    editor.putBoolean("ping_nearest", v)
    editor.commit()
  }

  def urbanMode = preferences.getBoolean("urban_mode", true)

  def urbanMode_=(v:Boolean) {
    val editor = preferences.edit()
    editor.putBoolean("urban_mode", v)
    editor.commit()
  }

}
