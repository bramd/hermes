package info.hermesnav.android

import android.content._
import android.net._
import android.preference._
import android.provider._

import info.hermesnav.core._

object Preferences {

  private var context:Context = null

  private lazy val preferences:SharedPreferences = PreferenceManager.getDefaultSharedPreferences(context)

  def apply(c:Context) {
    context = c
  }

  def intersectionNotificationTone = preferences.getString("intersection_notification_tone", "") match {
    case "" => Settings.System.DEFAULT_NOTIFICATION_URI
    case v => Uri.parse(v)
  }

  def pointNotificationTone = preferences.getString("point_notification_tone", "") match {
    case "" => Settings.System.DEFAULT_NOTIFICATION_URI
    case v => Uri.parse(v)
  }

  def measurementSystem = preferences.getString("measurementSystem", "metric") match {
    case "imperial" => Imperial
    case _ => Metric
  }

  def announceDirectionChanges_? = preferences.getBoolean("announceDirectionChanges", false)

  def announceAccuracyChanges_? = preferences.getBoolean("announceAccuracyChanges", false)

}
