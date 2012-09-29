package info.hermesnav
package core

import _root_.android.util.Log

import java.text.DecimalFormat

abstract class RelativeDirection(val description:String) {
  override val toString = description
}

case object Ahead extends RelativeDirection("ahead")
case object AheadAndLeft extends RelativeDirection("ahead and left")
case object LeftAndAhead extends RelativeDirection("left and ahead")
case object Left extends RelativeDirection("left")
case object LeftAndBehind extends RelativeDirection("left and behind")
case object BehindAndLeft extends RelativeDirection("behind and left")
case object Behind extends RelativeDirection("behind")
case object BehindAndRight extends RelativeDirection("behind and right")
case object RightAndBehind extends RelativeDirection("right and behind")
case object Right extends RelativeDirection("right")
case object RightAndAhead extends RelativeDirection("right and ahead")
case object AheadAndRight extends RelativeDirection("ahead and right")

case class Direction(val degrees:Double, relative:Boolean = false) {

  lazy val heading = normalize(degrees)

  def &(relative:Direction) = new Direction(normalize(relative.heading-heading), true)

  def toCardinalString = {
    if(heading <= 11.5)
      "north"
    else if(heading <= 34)
      "north northeast"
    else if(heading <= 56.5)
      "northeast"
    else if(heading <= 79)
      "east northeast"
    else if(heading <= 101.5)
      "east"
    else if(heading <= 124)
      "east southeast"
    else if(heading <= 146.5)
      "southeast"
    else if(heading <= 169)
      "south southeast"
    else if(heading <= 191.5)
      "south"
    else if(heading <= 214)
      "south southwest"
    else if(heading <= 236.5)
      "southwest"
    else if(heading <= 259)
      "west southwest"
    else if(heading <= 281.5)
      "west"
    else if(heading <= 304)
      "west northwest"
    else if(heading <= 326.5)
      "northwest"
    else if(heading <= 349)
      "north northwest"
    else
      "north"
  }

  def toRelativeDirection = {
    if(heading < 15) Ahead
    else if(heading < 45) AheadAndRight
    else if(heading < 75) RightAndAhead
    else if(heading < 105) Right
    else if(heading < 135) RightAndBehind
    else if(heading < 165) BehindAndRight
    else if(heading < 195) Behind
    else if(heading < 225) BehindAndLeft
    else if(heading < 255) LeftAndBehind
    else if(heading < 285) Left
    else if(heading < 315) LeftAndAhead
    else if(heading < 345) AheadAndLeft
    else Ahead
  }

  override def toString = if(relative) toRelativeDirection.toString else toCardinalString

  def toRoughRelativeString = {
    if(heading <= 30 || heading >= 330)
      "ahead"
    else if(heading >= 150 && heading <= 210)
      "behind"
    else if(heading < 150)
      "right"
    else
      "left"
  }

  private def normalize(v:Double):Double = v match {
    case v if(v >= 360) => normalize(v-360)
    case v if(v < 0) => normalize(v+360)
    case d => d
  }

}

sealed abstract class MeasurementSystem(val conversionFactor:Double)
case object Imperial extends MeasurementSystem(3.2808399)
case object Metric extends MeasurementSystem(0.3048)

case class Distance(val units:Double, val system:MeasurementSystem = Metric, standardized:Boolean = false) extends Ordered[Distance] {

  def to(target:MeasurementSystem) = target match {
    case v if(v == system) => this
    case v => Distance(units*v.conversionFactor, v)
  }

  override def toString = {
    val df = new DecimalFormat("#.#")
    system match {
      case Imperial =>
        if(units < 1056 && !standardized) {
          if(units == 1)
            "1 foot"
          else
            df.format(units)+" feet"
        } else {
          df.format(units/5280)+" miles"
        }
      case Metric =>
        if(units < 1000 && !standardized) {
          if(units == 1)
            "1 meter"
          else
            df.format(units)+" meters"
        } else
          df.format(units/1000)+" kilometers"
    }
  }

  def compare(that:Distance):Int = {
    if(units > that.units)
      1
    else if(units == that.units)
      0
    else
      -1
  }

  def per(time:Time) = Speed(this, time)

  def toDegreesAt(lat:Double) = {
    val v = to(Metric)
    (v.units/111133)+
    (v.units*math.abs(1.0/111200*math.cos(lat)))/2
  }

}

sealed trait Time
object Hour extends Time
object Minute extends Time
object Second extends Time

case class Speed(distance:Distance, time:Time = Hour) {

  override val toString = distance.copy(standardized = true)+" per "+(time match {
    case Second => "second"
    case Minute => "minute"
    case Hour => "hour"
  })

  def to(measurementSystem:MeasurementSystem) = Speed(distance.to(measurementSystem), time)

  def to(t:Time) = time match {
    case Second => t match {
      case Second => this
      case Minute => Speed(distance.copy(distance.units*60), Minute)
      case Hour => Speed(distance.copy(distance.units*3600), Hour)
    }
    case Minute => t match {
      case Second => Speed(distance.copy(distance.units/60), Second)
      case Minute => this
      case Hour => Speed(distance.copy(distance.units*60), Hour)
    }
    case Hour => t match {
      case Second => Speed(distance.copy(distance.units/3600), Second)
      case Minute => Speed(distance.copy(distance.units/60), Minute)
      case Hour => this
    }
  }

}

trait Position {

  val lat:Double

  val lon:Double

  def distanceTo(p:Position) = {
    val c = 6378
    val f1 = 21
    val angle = math.toRadians((lat+p.lat)/2)
    val f2 = (c-f1*math.sin(angle))*1000
    val p1lat = math.toRadians(lat)
    val p2lat = math.toRadians(p.lat)
    Distance(
      math.acos(
        math.sin(p1lat)*math.sin(p2lat)+math.cos(p1lat)*math.cos(p2lat) *
        math.cos(math.toRadians(p.lon-lon))
      )*f2
    )
  }

  def courseTo(p:Position) = {
    val dLat = p.lat-lat
    val dLon = p.lon-lon
    val alpha = math.atan2(dLat, dLon)*180/math.Pi
    val angle = if(alpha <= 90)
      90-alpha
    else
      450-alpha
    Direction(angle)
  }

}

case class Point(lat:Double, lon:Double) extends Position

trait RelativePosition extends Position {

  val perspective:Perspective

  val name:String

}

trait Path {

  val name:String

  val classification:Option[String]

  def crosses_?(other:Position):Boolean

  override val toString = name

}

trait IntersectionPosition extends RelativePosition {

  val paths:List[Path]

  def pathsExcept(path:Path):List[Path]

  def includes_?(path:Path):Boolean

  val neighbors:List[IntersectionPosition]

  lazy val name = {

    def countWays = neighbors.length match {
      case 1 => "Dead end"
      case v => v+"-way intersection"
    }

    val myPaths =
      perspective.nearestPath.map { np =>
        if(includes_?(np))
          np :: pathsExcept(np)
        else paths
      }.getOrElse(paths)

    val pathsToSentence = paths.size match {
      case 2 =>
        val first = myPaths.head
        val second = myPaths.reverse.head
        Log.d("hermescheck40", "Cross check: "+paths.mkString(", "))
        first.name+" "+(if(first.crosses_?(this))
          "crossing"
        else
          "and"
        )+" "+second.name
      case _ => myPaths.map(_.name).mkString(", ")
    }

    countWays+": "+pathsToSentence
  }

  override def toString = name

}

trait PointOfInterest extends RelativePosition {

  val name:String

  val classification:Option[String]

}

trait Perspective extends Position {

  val direction:Option[Direction]

  protected val nearestPathThreshold = 30 meters

  val nearestPath:Option[Path]

  protected def calcNearestPath:Option[Path] =
    previous.flatMap(_.nearestIntersection)
    .find(i => distanceTo(i).to(Metric) <= (30 meters))
    .flatMap(v => previous.get.nearestPath)

  val speed:Speed

  val timestamp:Long

  protected var previous:Option[Perspective]

  protected def nearestIntersectionDistance = 40 meters

  protected val nearestIntersectionCandidates:List[IntersectionPosition]

  lazy val nearestIntersection:Option[IntersectionPosition] = {
    Log.d("hermescheck", "All: "+nearestIntersectionCandidates)
    val distance = (30 meters)
    val candidates = nearestPath.map { np =>
      nearestIntersectionCandidates.filter(_.includes_?(np))
    }.getOrElse(nearestIntersectionCandidates.sortBy(distanceTo(_)))
    .filter { c =>
      c.distanceTo(this) <= distance
    }
    candidates.find { c =>
      bearingTo(c).map { b =>
        List(Ahead, AheadAndLeft, AheadAndRight).contains(b.toRelativeDirection)
      }.getOrElse(false)
    }.orElse(candidates.headOption)
  }

  def nearestPoints(limit:Int = 10, skip:Int = 0):List[PointOfInterest]

  def bearingTo(p:Position) = direction.map(_ & courseTo(p))

}
