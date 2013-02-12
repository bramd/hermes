package info.hermesnav.android
package events

import collection.mutable.ListBuffer

import info.hermesnav.core._

class Event[T] {

  private val handlers = ListBuffer[(T) => Any]()

  def add(h:(T) => Any) =
    handlers += h

  def +=(h:(T) => Any) =
    add(h)

  def add(h: => Any) =
    handlers += { (Unit) => h }

  def +=(h: => Any) =
    add(h)

  def remove(h:(T) => Any) =
    handlers -= h

  def -=(h:(T) => Any) =
    remove(h)

  def remove(h: => Any) =
    handlers -= { (Unit) => h }

  def -=(h: => Any) =
    remove(h)

  def apply(arg:T) = handlers.foreach(_(arg))

}

object AccuracyChanged extends Event[Option[Distance]]

object CompassEnabled extends Event[Boolean]

object DirectionChanged extends Event[Option[Direction]]

object NearestIntersectionChanged extends Event[Option[IntersectionPosition]]

object NearestPathChanged extends Event[Option[Path]]

object NearestPoints extends Event[List[PointOfInterest]]

object PerspectiveChanged extends Event[Perspective]

object ProviderChanged extends Event[Option[String]]

object SpeedChanged extends Event[Option[Speed]]
