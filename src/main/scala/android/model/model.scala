package info.hermesnav.android
package model

import collection.SortedSet

import android.util.Log

import jsqlite._

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

case class AndroidMap(val features:Database, val graph:Database) {
  def close() {
    features.close()
    graph.close()
  }
}

case class AndroidPath(map:AndroidMap, ids:SortedSet[Int], val name:Option[String], val classification:Map[String, String], geom:String) extends Path with Namer {

  private[model] def joinWith(other:AndroidPath) = {
    var rv:AndroidPath = null
    map.features.exec(
      "select asWKT(gUnion(geomFromText('"+geom+"'), geomFromText('"+other.geom+"'))) as geom",
      { row:Map[String, String] =>
        rv = copy(ids = (other.ids++ids), classification = classification++other.classification, geom = row("geom"))
        false
      }
    )
    rv
  }

  def crosses_?(other:Position) = {
    var rv = false
    map.features.exec(
      "select Crosses(geomFromText('"+geom+"'), ST_Buffer(MakePoint("+other.lon+", "+other.lat+"), 0.0001)) as crosses",
      { row:Map[String, String] =>
        if(row("crosses").toInt == 1)
          rv = true
        false
      }
    )
    rv
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
    map.features.exec(
      "select name, asWKT(GEOMETRY) as geom, * from lines where osm_id = "+id+" limit 1",
      { row:Map[String, String] =>
        rv = Some(new AndroidPath(map, SortedSet(id), row.get("name"), classify(row), row("geom")))
        false
      }
    )
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
          rv ::= same.joinWith(path)
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

case class AndroidPointOfInterest(val perspective:Perspective, val classification:Map[String, String], val lat:Double, val lon:Double) extends PointOfInterest with Namer {

  val name = classification.get("name").orElse(classification.get("ref")).orElse(classification.get("amenity")).orElse(classification.get("shop")).orElse(classification.get("highway")).map(humanizeUnderscoredString(_)).getOrElse("Unnamed")

  override def toString = name+": "+distanceTo(perspective).to(Preferences.measurementSystem)+perspective.bearingTo(this).map(" "+_).getOrElse("")

}

class AndroidPerspective(maps:List[AndroidMap], val lat:Double, val lon:Double, val direction:Option[Direction], val speed:Speed, val timestamp:Long, var previous:Option[Perspective]) extends Perspective with Classifier with Namer {

  def nearestPoints(searchRadius:Distance = defaultPointSearchRadius, limit:Int = 10, skip:Int = 0) = {
    var rv = List[PointOfInterest]()
    maps.map(_.features.exec(
      """select *,
      X(Centroid(GEOMETRY)) as lon, Y(Centroid(GEOMETRY)) as lat,
      Distance(Centroid(GEOMETRY), MakePoint("""+lon+""", """+lat+""")) as distance
      from points
      where rowid in (
        select rowid from SpatialIndex
        where f_table_name = 'points'
        and f_geometry_column = 'GEOMETRY'
        and search_frame = BuildCircleMBR("""+lon+""", """+lat+""", """+searchRadius.toDegreesAt(lat)+"""))
      order by distance limit """+limit,
      { row:Map[String, String] =>
        rv ::= AndroidPointOfInterest(this, classify(row), row("lat").toDouble, row("lon").toDouble)
        false
      }
    ))
    rv.sortBy(distanceTo(_))
  }

  val nearestIntersectionCandidates = {
    var rv = List[IntersectionPosition]()
    maps.map { m =>
      m.graph.exec(
        """select Distance(geometry, MakePoint("""+lon+""", """+lat+""")) as distance,
        X(geometry) as lon,
        Y(geometry) as lat, node_id as id
        from paths_nodes
        where paths_nodes.rowid in (
          select rowid from SpatialIndex
          where f_table_name = 'paths_nodes'
          and f_geometry_column = 'geometry'
          and search_frame = BuildCircleMBR("""+lon+""", """+lat+""", """+nearestIntersectionDistance.toDegreesAt(lat)+""")
        ) order by distance""",
        { row:Map[String, String] =>
          rv ::= new AndroidIntersectionPosition(m, row("id").toInt, this, row("lat").toDouble, row("lon").toDouble)
          false
        }
      )
    }
    rv.sortBy(distanceTo(_))
  }

  val nearestPath:Option[Path] = {
    calcNearestPath.orElse {
      val distance = nearestPathThreshold.toDegreesAt(lat)
      var paths:List[AndroidPath] = Nil
      maps.map { m =>
        m.features.exec(
          """select osm_id, Distance(GEOMETRY, MakePoint("""+lon+""", """+lat+""")) as distance, name, asWKT(GEOMETRY) as geom, *
          from lines
          where lines.rowid in (
            select rowid from SpatialIndex
            where f_table_name = 'lines'
            and f_geometry_column = 'GEOMETRY'
            and search_frame = BuildCircleMBR("""+lon+""", """+lat+""", """+distance+""")
          ) and distance <= """+distance+""" order by distance limit 1""",
          { row:Map[String, String] =>
            paths ::= AndroidPath(m, SortedSet(row("osm_id").toInt), row.get("name"), classify(row), row("geom"))
            false
          }
        )
      }
      (for(
        p <- previous;
        pnp <- p.nearestPath;
        pnp2 = pnp if(paths.contains(pnp))
      ) yield(pnp2))
      .orElse(paths.reverse.headOption)
    }
  }

  finish()

}
