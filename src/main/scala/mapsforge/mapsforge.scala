package info.hermesnav.mapsforge

import collection.JavaConversions._

import android.util.Log

import org.mapsforge.core.model._
import org.mapsforge.map.reader.{MapDatabase, Way}
import org.mapsforge.core.util.MercatorProjection

import info.hermesnav.android.preferences._
import info.hermesnav.core._

trait Namer {
  protected def humanizeUnderscoredString(str:String) =
    str.replace("_", " ")
}

trait Classifier {
  def classify(row:Map[String, String]) = {
    var classification = Map[String, String]()
    def addClassification(name:String) =
      row.get(name).foreach { v =>
        if(!v.isEmpty)
          classification += (name -> v)
      }
    List("amenity", "barrier", "class", "highway", "man_made", "name", "place", "shop")
    .foreach(addClassification(_))
    classification ++= row.get("other_tags").map { tags =>
      tags.split(",")
      .map(_.split("=>"))
      .map(v => (v.head.trim, v.last.trim))
      .toMap
    }.getOrElse(Map.empty)
    classification
  }
}

case class MapsforgeIdentifier(val source:String, val identifier:Long) extends Identifier 

case class MapsforgePointOfInterest(val perspective:Perspective, val classification:Map[String, String], val lat:Double, val lon:Double, val identifier:Identifier) extends PointOfInterest with Namer {

  val name = classification.get("name").orElse(classification.get("ref")).orElse(classification.get("amenity")).orElse(classification.get("shop")).orElse(classification.get("highway")).map(humanizeUnderscoredString(_)).getOrElse("Unnamed")

  override def toString = name+": "+distanceTo(perspective).to(Preferences.measurementSystem)+perspective.bearingTo(this).map(" "+_).getOrElse("")

}

case class MapsforgePath(way:Way) extends Path with Namer {

  lazy val classification = Map(way.tags.map(v => v.key -> v.value) :_*)

  lazy val name = classification.get("name")

  def crosses_?(other:Position) = false // FIXME

  override val toString =
    name
    .orElse(classification.get("service"))
    .orElse(classification.get("highway"))
    .orElse(classification.get("railway"))
    .orElse(classification.get("waterway"))
    .orElse(classification.get("class"))
    .map(humanizeUnderscoredString(_))
    .getOrElse("Unnamed")

  override val hashCode = way.hashCode

  override def equals(other:Any) = other match {
    case o:MapsforgePath =>
      val latLongs = way.latLongs.toList.flatten.map(v => v.latitude -> v.longitude)
      val oLatLongs = o.way.latLongs.toList.flatten.map(v => v.latitude -> v.longitude)
      latLongs == oLatLongs
    case _ => false
  }

}

case class MapsforgeLatLongPosition(position:LatLong) extends Position {
  val lat = position.latitude
  val lon = position.longitude
}

case class MapsforgeIntersectionPosition(val perspective:MapsforgePerspective, v:Tuple2[LatLong, Set[Way]]) extends IntersectionPosition {

  val lat = v._1.latitude
  val lon = v._1.longitude

  lazy val paths:Set[Path] =
    v._2.map(MapsforgePath(_))

  lazy val neighbors:Set[Position] =
    v._2.map { way =>
      val latLongs = way.latLongs.toList.head // FIXME Why is it an array of arrays?
      if(v._1 == latLongs(0) || v._1 == latLongs(latLongs.length-1))
        Seq[Position]()
      else {
        val index = latLongs.indexOf(v._1)
        Seq(MapsforgeLatLongPosition(latLongs(index-1)), MapsforgeLatLongPosition(latLongs(index+1)))
      }
    }.flatten

}

class MapsforgePerspective(maps:List[MapDatabase], val lat:Double, val lon:Double, val direction:Option[Direction], val speed:Speed, val timestamp:Long, var previous:Option[Perspective]) extends Perspective with Classifier with Namer {

  val zoomLevel:Byte = 17
  val tile = new Tile(MercatorProjection.longitudeToTileX(lon, zoomLevel),
                         MercatorProjection.latitudeToTileY(lat, zoomLevel), zoomLevel)

  def nearestPoints(searchRadius:Distance = defaultPointSearchRadius, limit:Int = 10, skip:Int = 0,
                    filter: (PointOfInterest) => Boolean = { p => true }) = {
                      Log.d("hermescheck", "nearestPoints")
    maps.flatMap(_.readMapData(tile).pointOfInterests.map { p =>
      new MapsforgePointOfInterest(this, p.tags.map({ t => (t.key, t.value) }).toMap,
                                 p.position.latitude, p.position.longitude,
                                 new MapsforgeIdentifier("osm", p.hashCode))
    }).filter(filter).sortBy(distanceTo(_))
  }

  val nearestIntersectionCandidates:Set[IntersectionPosition] = {
    var latLongToWays = collection.mutable.Map[LatLong, collection.mutable.Set[Way]]()
    val ways = maps.flatMap(_.readMapData(tile).ways)
    maps.foreach(_.readMapData(tile).ways.map { w =>
      w.latLongs.flatten.foreach { ll => 
        latLongToWays.get(ll).map { x =>
          latLongToWays(ll) += w
        }.getOrElse(latLongToWays(ll) = collection.mutable.Set(w))
      }
    })
    latLongToWays.filter (v => v._2.size > 1 )
    .map(v => (v._1, v._2.toSet))
    .map(MapsforgeIntersectionPosition(this, _)).toSet
  }

  val nearestPath:Option[Path] = {
    calcNearestPath.orElse {
      val wayPoints = maps.flatMap(_.readMapData(tile).ways.map { way =>
        (way, way.latLongs.toList.flatten)
      })
      def toSegments(points:List[LatLong]):List[Tuple2[LatLong, LatLong]] = points match {
        case Nil => Nil
        case hd :: Nil => Nil
        case fst :: snd :: rest => (fst, snd) :: toSegments(rest)
      }
      wayPoints.map(v => (v._1, toSegments(v._2)))
      .map { v =>
        def distanceTo(seg:Tuple2[LatLong, LatLong]) = {
          val rNum = (lat-seg._1.latitude)*(seg._2.latitude-seg._1.latitude)+(lon-seg._1.longitude)*(seg._2.longitude-seg._1.longitude)
          val rDenom = math.pow(seg._2.latitude-seg._1.latitude, 2)+math.pow(seg._2.longitude-seg._1.longitude, 2)
          val r = rNum/rDenom
          val px = seg._1.latitude+r*(seg._2.latitude-seg._1.latitude)
          val py = seg._1.longitude+r*(seg._2.longitude-seg._1.longitude)
          val s = ((seg._1.longitude-lon)*(seg._2.latitude-seg._1.latitude)-(seg._1.latitude-lat)*(seg._2.longitude-seg._1.longitude))/rDenom
          s.abs*math.sqrt(rDenom)
        }
        (v._1, v._2.map(distanceTo(_)).min)
      }.sortWith((v1:Tuple2[Way, Double], v2:Tuple2[Way, Double]) => v1._2 < v2._2)
      .headOption.map(v => MapsforgePath(v._1))
    }
  }

  Log.d("hermes", "finishing")
  finish()

}
