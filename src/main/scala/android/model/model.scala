package info.hermesnav.android
package model

import collection.SortedSet
import collection.JavaConversions._

import android.util.Log

import jsqlite._

import com.graphhopper.GraphHopper

import org.mapsforge.map.reader.MapDatabase
import org.mapsforge.core.util.MercatorProjection
import org.mapsforge.core.model.Tile

import info.hermesnav.core._
import preferences._

private[model] class CB(cb:Map[String, String] => Boolean) extends Callback {

  private var columns:List[String] = Nil

  private var types:List[String] = Nil

  def columns(c:Array[String]) {
    columns = c.toList
  }

  def types(t:Array[String]) {
    types = t.toList
  }

  def newrow(row:Array[String]) = {
    cb(
      columns.zip(row.toList).filter(v => v._2 != null && !v._2.trim.isEmpty).toMap
    )
  }

}

case class AndroidMap(val features:MapDatabase, val graph:Database) {
  def close() {
    graph.close()
  }
}

case class AndroidPath(map:AndroidMap, ids:SortedSet[Int], val name:Option[String], val classification:Map[String, String], geom:String) extends Path with Namer {

  private[model] def joinWith(other:AndroidPath) = {
    None
  }

  def crosses_?(other:Position) = {
    false
  }

  override val toString =
    name
    .orElse(classification.get("service"))
    .orElse(classification.get("highway"))
    .orElse(classification.get("railway"))
    .orElse(classification.get("waterway"))
    .orElse(classification.get("class"))
    .map(humanizeUnderscoredString(_))
    .getOrElse("Unnamed")

}

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

object AndroidPath extends Classifier {
  def apply(map:AndroidMap, id:Int):Option[AndroidPath] = {
    var rv:Option[AndroidPath] = None
    rv
  }
}

class AndroidIntersectionPosition(private val map:AndroidMap, private val id:Int, val perspective:Perspective, val lat:Double, val lon:Double) extends IntersectionPosition with Classifier {

  lazy val paths = {
    var rv = List[AndroidPath]()
    map.graph.exec(
      "select osm_id, name, class, asWKT(geometry) as geom from paths where node_from = "+id+" or node_to = "+id,
      { row:Map[String, String] =>
        val path = AndroidPath(map, row("osm_id").toInt).getOrElse {
          new AndroidPath(map, SortedSet(row("osm_id").toInt), row.get("name"), classify(row), row("geom"))
        }
        rv.find { v =>
          v.name != None && v.name == path.name
        }.headOption.map { same =>
          rv = rv.filterNot(_ == same)
//          rv ::= same.joinWith(path)
        }.getOrElse {
          rv ::= path
        }
        false
      }
    )
    rv.distinct
  }

  def pathsExcept(path:Path) = {
    val ids = path.asInstanceOf[AndroidPath].ids
    paths.filter { p =>
      var rv = true
      ids.foreach { id =>
        if(p.asInstanceOf[AndroidPath].ids.contains(id))
          rv = false
      }
      rv
    }
  }

  def includes_?(path:Path) = path match {
    case p:AndroidPath =>
      val ids = paths.map(_.ids).flatten.distinct
      var rv = false
      p.ids.foreach { id =>
        if(ids.contains(id))
          rv = true
      }
      rv
    case _ => false
  }

  lazy val neighbors = {
    var ids = List[Int]()
    map.graph.exec(
      "select osm_id, node_from, node_to from paths where node_from = "+id+" or node_to = "+id,
      { row:Map[String, String] =>
        val from = row("node_from").toInt
        val to = row("node_to").toInt
        ids ::= (if(from == id) to else from)
        false
      }
    )
    ids.map { id =>
      AndroidIntersectionPosition(map, id, perspective)
    }.flatten
  }

  override def hashCode =
    41*41*41+map.features.hashCode+map.graph.hashCode+id.hashCode

  override def equals(o:Any) = o match {
    case i:AndroidIntersectionPosition if(map == i.map && id == i.id) => true
    case _ => false
  }

}

object AndroidIntersectionPosition {
  def apply(map:AndroidMap, id:Int, perspective:Perspective):Option[AndroidIntersectionPosition] = {
    var rv:Option[AndroidIntersectionPosition] = None
    map.graph.exec(
      "select X(geometry) as lon, Y(geometry) as lat from paths_nodes where node_id = "+id+" limit 1",
      { row:Map[String, String] =>
        rv = Some(new AndroidIntersectionPosition(map, id, perspective, row("lat").toDouble, row("lon").toDouble))
        false
      }
    )
    rv
  }
}

case class AndroidIdentifier(val source:String, val identifier:Long) extends Identifier {

}

case class AndroidPointOfInterest(val perspective:Perspective, val classification:Map[String, String], val lat:Double, val lon:Double, val identifier:Identifier) extends PointOfInterest with Namer {

  val name = classification.get("name").orElse(classification.get("ref")).orElse(classification.get("amenity")).orElse(classification.get("shop")).orElse(classification.get("highway")).map(humanizeUnderscoredString(_)).getOrElse("Unnamed")

  override def toString = name+": "+distanceTo(perspective).to(Preferences.measurementSystem)+perspective.bearingTo(this).map(" "+_).getOrElse("")

}

class AndroidPerspective(maps:List[AndroidMap], val lat:Double, val lon:Double, val direction:Option[Direction], val speed:Speed, val timestamp:Long, var previous:Option[Perspective]) extends Perspective with Classifier with Namer {

  def nearestPoints(searchRadius:Distance = defaultPointSearchRadius, limit:Int = 10, skip:Int = 0,
                    filter: (PointOfInterest) => Boolean = { p => true }) = {
    val zoomLevel:Byte = 18
    val tile = new Tile(MercatorProjection.longitudeToTileX(lon, zoomLevel),
                         MercatorProjection.latitudeToTileY(lat, zoomLevel), zoomLevel)
    maps.flatMap(_.features.readMapData(tile).pointOfInterests.map(p =>
      new AndroidPointOfInterest(this, p.tags.map({ t => (t.key, t.value) }).toMap,
                                 p.position.latitude, p.position.longitude,
                                 new AndroidIdentifier("osm", p.hashCode))
    )).filter(filter).sortBy(distanceTo(_))
  }

  Log.d("hermes", "nearestIntersectionCandidates")
  val nearestIntersectionCandidates = {
    var rv = List[IntersectionPosition]()
//    maps.map { m =>
//      m.graph.exec(
//        """select Distance(geometry, MakePoint("""+lon+""", """+lat+""")) as distance,
//        X(geometry) as lon,
//        Y(geometry) as lat, node_id as id
//        from paths_nodes
//        where paths_nodes.rowid in (
//          select rowid from SpatialIndex
//          where f_table_name = 'paths_nodes'
//          and f_geometry_column = 'geometry'
//          and search_frame = BuildCircleMBR("""+lon+""", """+lat+""", """+nearestIntersectionDistance.toDegreesAt(lat)+""")
//        ) order by distance""",
//        { row:Map[String, String] =>
//          rv ::= new AndroidIntersectionPosition(m, row("id").toInt, this, row("lat").toDouble, row("lon").toDouble)
//          false
//        }
//      )
//    }
    rv.sortBy(distanceTo(_))
  }

  Log.d("hermes", "nearestPathBegin")
  val nearestPath:Option[Path] = {
    calcNearestPath.orElse(None)
  }

  Log.d("hermes", "finishing")
  finish()

}
