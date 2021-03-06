package info.hermesnav
package core

import _root_.android.util.Log

import java.text.DecimalFormat

import language.postfixOps

object CoarseRelativeDirection extends Enumeration {
  val Ahead = Value("ahead")
  val Left = Value("left")
  val Behind = Value("behind")
  val Right = Value("right")
}

object FineRelativeDirection extends Enumeration {
  val Ahead = Value("ahead")
  val AheadAndLeft = Value("ahead and left")
  val LeftAndAhead = Value("left and ahead")
  val Left = Value("left")
  val LeftAndBehind = Value("left and behind")
  val BehindAndLeft = Value("behind and left")
  val Behind = Value("behind")
  val BehindAndRight = Value("behind and right")
  val RightAndBehind = Value("right and behind")
  val Right = Value("right")
  val RightAndAhead = Value("right and ahead")
  val AheadAndRight = Value("ahead and right")
}

object CoarseCardinalDirection extends Enumeration {
  val North = Value("north")
  val Northeast = Value("northeast")
  val East = Value("east")
  val Southeast = Value("southeast")
  val South = Value("south")
  val Southwest = Value("southwest")
  val West = Value("west")
  val Northwest = Value("northwest")
}

object FineCardinalDirection extends Enumeration {
  val North = Value("north")
  val NorthNortheast = Value("north northeast")
  val Northeast = Value("northeast")
  val EastNortheast = Value("east northeast")
  val East = Value("east")
  val EastSoutheast = Value("east southeast")
  val Southeast = Value("southeast")
  val SouthSoutheast = Value("south southeast")
  val South = Value("south")
  val SouthSouthwest = Value("south southwest")
  val Southwest = Value("southwest")
  val WestSouthwest = Value("west southwest")
  val West = Value("west")
  val WestNorthwest = Value("west northwest")
  val Northwest = Value("northwest")
  val NorthNorthwest = Value("north northwest")
}

case class Direction(val degrees:Double, relative:Boolean = false) {

  lazy val heading = normalize(degrees)

  def &(relative:Direction) = new Direction(normalize(relative.heading-heading), true)

  lazy val coarseCardinalDirection = {
    import CoarseCardinalDirection._
    if(heading <= 22.5)
      North
    else if(heading <= 67.5)
      Northeast
    else if(heading <= 112.5)
      East
    else if(heading <= 157.5)
      Southeast
    else if(heading <= 202.5)
      South
    else if(heading <= 247.5)
      Southwest
    else if(heading <= 292.5)
      West
    else if(heading <= 337.5)
      Northwest
    else
      North
  }

  lazy val fineCardinalDirection = {
    import FineCardinalDirection._
    if(heading <= 11.5)
      North
    else if(heading <= 34)
      NorthNortheast
    else if(heading <= 56.5)
      Northeast
    else if(heading <= 79)
      EastNortheast
    else if(heading <= 101.5)
      East
    else if(heading <= 124)
      EastSoutheast
    else if(heading <= 146.5)
      Southeast
    else if(heading <= 169)
      SouthSoutheast
    else if(heading <= 191.5)
      South
    else if(heading <= 214)
      SouthSouthwest
    else if(heading <= 236.5)
      Southwest
    else if(heading <= 259)
      WestSouthwest
    else if(heading <= 281.5)
      West
    else if(heading <= 304)
      WestNorthwest
    else if(heading <= 326.5)
      Northwest
    else if(heading <= 349)
      NorthNorthwest
    else
      North
  }

  lazy val fineRelativeDirection = {
    import FineRelativeDirection._
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

  override def toString = if(relative) fineRelativeDirection.toString else fineCardinalDirection.toString

  lazy val coarseRelativeDirection = {
    import CoarseRelativeDirection._
    if(heading <= 30 || heading >= 330)
      Ahead
    else if(heading >= 150 && heading <= 210)
      Behind
    else if(heading < 150)
      Right
    else
      Left
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
          if(units.toInt == 1)
            "1 foot"
          else
            units.toInt+" feet"
        } else {
          val str = df.format(units/5280)
          if(str == "1" || units == 5280)
            "1 mile"
          else
            str+" miles"
        }
      case Metric =>
        if(units < 1000 && !standardized) {
          if(units.toInt == 1)
            "1 meter"
          else
            units.toInt+" meters"
        } else {
          val str = df.format(math.round(units/1000))
          if(str == "1" || units == 1000)
            "1 kilometer"
          else
            str+" kilometers"
        }
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

  def +(n:Double) = copy(units+n)

  def -(n:Double) = copy(units-n)

  def *(n:Double) = copy(units*n)

  def /(n:Double) = copy(units/n)

}

sealed trait Time
object Hour extends Time
object Minute extends Time
object Second extends Time

case class Speed(distance:Distance, time:Time = Hour) extends Ordered[Speed] {

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

  def compare(other:Speed) = {
    val me = to(Metric).to(seconds)
    val them = other.to(Metric).to(seconds)
    me.distance.compare(them.distance)
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

trait RelativePosition extends Position {

  val perspective:Perspective

  val name:String

}

trait Path {

  val name:Option[String]

  val classification:Map[String, String]

  def crosses_?(other:Position):Boolean

  override val toString = name.getOrElse("Unnamed")

}

trait IntersectionPosition extends RelativePosition {

  val paths:Set[Path]

  def pathsExcept(path:Path):Set[Path] = paths.filterNot(_ == path)

  def includes_?(path:Path):Boolean = paths.contains(path)

  val neighbors:Set[Position]

  lazy val name = {

    def countWays = neighbors.size match {
      case 1 => "Dead end"
      case v => v+"-way intersection"
    }

    val myPaths =
      perspective.nearestPath.map { np =>
        if(includes_?(np))
          pathsExcept(np)+np
        else paths
      }.getOrElse(paths).toSet

    val pathsToSentence = paths.size match {
      case 2 =>
        val first = myPaths.head
        val second = myPaths.last
        Log.d("hermescheck40", "Cross check: "+paths.mkString(", "))
        first.toString+" "+(if(first.crosses_?(this))
          "crossing"
        else
          "and"
        )+" "+second
      case _ => toSentence(myPaths.map(_.toString).toList)
    }

    countWays+": "+pathsToSentence
  }

  override def toString = name

}

trait Identifier {

  val source:String
  val identifier:Long

  override def toString(): String = source + "_" + identifier.toString;
}

trait PointOfInterest extends RelativePosition {

  val name:String

  val classification:Map[String, String]
  val identifier:Identifier

}

trait Perspective extends Position {

  val direction:Option[Direction]

  protected val nearestPathThreshold = 30 meters

  val nearestPath:Option[Path]

  private val newPathThreshold = 30 meters

  protected def calcNearestPath:Option[Path] =
    for(
      p <- previous;
      pnp <- p.nearestPath;
      ni <- nearestIntersectionCandidates.find { i =>
        distanceTo(i) <= (newPathThreshold) &&
        i.includes_?(pnp)
      }
    ) yield(pnp)

  val speed:Speed

  val timestamp:Long

  protected var previous:Option[Perspective]

  def finish() =
    previous.foreach(_.previous = None)

  private val vehicularModeThreshold = 15 kph

  private val lastSpeedDrop:Option[Long] = {
    if(speed >= vehicularModeThreshold) None else previous.flatMap { p =>
      if(p.speed >= vehicularModeThreshold) Some(System.currentTimeMillis)
      else p.lastSpeedDrop
    }
  }

  val vehicular = if(speed >= vehicularModeThreshold)
    true
  else lastSpeedDrop.map { l =>
    if(System.currentTimeMillis-l <= 60000)
      true
    else
      false
  }.getOrElse(false)

  protected val nearestIntersectionDistance = if(vehicular)
    200 meters
  else
    40 meters

  protected val nearestIntersectionCandidates:Set[IntersectionPosition]

  lazy val nearestIntersection:Option[IntersectionPosition] = {
    val candidates = nearestPath.map { np =>
      nearestIntersectionCandidates.filter(_.includes_?(np))
    }.getOrElse(nearestIntersectionCandidates)
    .filter(distanceTo(_) <= nearestIntersectionDistance)
    .toList.sortBy(distanceTo(_))
    previous.flatMap(_.nearestIntersection).find { ni =>
      distanceTo(ni) <= (20 meters)
    }.orElse {
      candidates.find { c =>
        bearingTo(c).map { b =>
          import FineRelativeDirection._
          List(Ahead, AheadAndLeft, AheadAndRight).contains(b.fineRelativeDirection)
        }.getOrElse(false)
      }
    }.orElse(candidates.headOption)
  }

  protected def defaultPointSearchRadius = 2 kilometers

  def nearestPoints(searchRadius:Distance = defaultPointSearchRadius, limit:Int = 10, skip:Int = 0, filter: (PointOfInterest) => Boolean = { p => true }):List[PointOfInterest]

  def bearingTo(p:Position) = direction.map(_ & courseTo(p))

}
