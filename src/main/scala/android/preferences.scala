package info.hermesnav.android
package preferences

import android.content._
import android.net._
import android.preference._
import android.provider._

import info.hermesnav.core._

object DirectionAnnouncementPrecision extends Enumeration {
  val Off = Value("off")
  val Low = Value("low")
  val High = Value("high")
}

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

  def directionAnnouncementPrecision =
    DirectionAnnouncementPrecision.withName(preferences.getString("directionAnnouncementPrecision", "off"))

  def announceAccuracyChanges_? =
    preferences.getBoolean("announceAccuracyChanges", false)

}
