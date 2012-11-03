package info.hermesnav.android
package model

import android.util.Log

import jsqlite._

import info.hermesnav.core._

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
      columns.zip(row.toList).filter(_._2 != null).toMap
    )
  }

}

case class AndroidMap(val features:Database, val graph:Database) {
  def close() {
    features.close()
    graph.close()
  }
}

case class AndroidPath(map:AndroidMap, ids:List[Int], val name:Option[String], val classification:Option[String], geom:String) extends Path with PathNamer {

  private[model] def joinWith(other:AndroidPath) = {
    var rv:AndroidPath = null
    map.features.exec(
      "select asWKT(gUnion(geomFromText('"+geom+"'), geomFromText('"+other.geom+"'))) as geom",
      { row:Map[String, String] =>
        rv = copy(ids = (other.ids++ids).distinct, geom = row("geom"))
        false
      }
    )
    rv
  }

  def crosses_?(other:Position) = {
    var rv = false
    Log.d("hermescheck40", "Checking "+name)
    map.features.exec(
      "select Crosses(geomFromText('"+geom+"'), ST_Buffer(MakePoint("+other.lon+", "+other.lat+"), 0.0001)) as crosses",
      { row:Map[String, String] =>
        Log.d("hermescheck40", "Got "+row)
        if(row("crosses").toInt == 1)
          rv = true
        false
      }
    )
    rv
  }

  override val toString =
    name
    .orElse(classification.map(humanizeUnderscoredString(_)))
    .getOrElse("Unnamed")

}

trait Namer {
  protected def humanizeUnderscoredString(str:String) =
    str.replace("_", " ")
}

trait PathNamer extends Namer {
  protected def namePath(row:Map[String, String]) =
    row.get("name")
}

object AndroidPath extends PathNamer {
  def apply(map:AndroidMap, id:Int):Option[AndroidPath] = {
    var rv:Option[AndroidPath] = None
    map.features.exec(
      "select name, highway, asWKT(GEOMETRY) as geom from lines where osm_id = "+id+" limit 1",
      { row:Map[String, String] =>
        rv = Some(new AndroidPath(map, List(id), namePath(row), row.get("highway"), row("geom")))
        false
      }
    )
    rv
  }
}

class AndroidIntersectionPosition(private val map:AndroidMap, private val id:Int, val perspective:Perspective, val lat:Double, val lon:Double) extends IntersectionPosition with PathNamer {

  lazy val paths = {
    var rv = List[AndroidPath]()
    map.graph.exec(
      "select osm_id, name, class, asWKT(geometry) as geom from paths where node_from = "+id+" or node_to = "+id,
      { row:Map[String, String] =>
        val path = AndroidPath(map, row("osm_id").toInt).getOrElse {
          new AndroidPath(map, List(row("osm_id").toInt), namePath(row), row.get("class"), row("geom"))
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

  def pathsExcept(path:Path) = paths.filterNot(_.name == path.name)

  def includes_?(path:Path) = path match {
    case p:AndroidPath =>
      val ids:List[Int] = paths.map(_.ids).flatten.distinct
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
    }.filterNot(_ == None).map(_.get)
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

case class AndroidPointOfInterest(val perspective:Perspective, val name:String, val classification:Map[String, String], val lat:Double, val lon:Double) extends PointOfInterest {
  override val toString = name+": "+distanceTo(perspective)+perspective.bearingTo(this).map(" "+_).getOrElse("")
}

class AndroidPerspective(maps:List[AndroidMap], val lat:Double, val lon:Double, val direction:Option[Direction], val speed:Speed, val timestamp:Long, var previous:Option[Perspective]) extends Perspective with PathNamer {

  def nearestPoints(searchRadius:Distance = defaultPointSearchRadius, limit:Int = 10, skip:Int = 0) = {
    var rv = List[PointOfInterest]()
    maps.map(_.features.exec(
      """select *,
      X(Centroid(GEOMETRY)) as lon, Y(Centroid(GEOMETRY)) as lat,
      Distance(Centroid(GEOMETRY), MakePoint("""+lon+""", """+lat+""")) as distance
      from multipolygons
      where rowid in (
        select rowid from SpatialIndex
        where f_table_name = 'multipolygons'
        and f_geometry_column = 'GEOMETRY'
        and search_frame = BuildCircleMBR("""+lon+""", """+lat+""", """+searchRadius.toDegreesAt(lat)+"""))
      order by distance limit """+limit,
      { row:Map[String, String] =>
        var classification = Map[String, String]()
        def addClassification(name:String) =
          row.get(name).foreach { v =>
            if(!v.isEmpty)
              classification += (name -> v)
          }
        List("aeroway", "amenity", "barrier", "boundary", "building", "craft", "geological", "historic", "land_area", "landuse", "leisure", "man_made", "military", "natural", "office", "place", "shop", "sport", "tourism", "type")
        .foreach(addClassification(_))
        row.get("name").getOrElse {
          true
        }
        def multiTypeString(str:String) =
          str.split(";").map(_.trim).mkString("/")
        val n:String = row.get("name")
        .orElse(row.get("amenity"))
        .orElse(row.get("leisure").map { leisure =>
          leisure match {
            case "pitch" => row.get("sport").map(multiTypeString(_)+" ").getOrElse("")+"field"
            case v => v
          }
        }).getOrElse("unnamed")
        rv ::= AndroidPointOfInterest(this, n, classification, row("lat").toDouble, row("lon").toDouble)
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
      var rv:Option[Path] = None
      maps.map { m =>
        m.features.exec(
          """select osm_id, Distance(GEOMETRY, MakePoint("""+lon+""", """+lat+""")) as distance, name, highway, asWKT(GEOMETRY) as geom
          from lines
          where lines.rowid in (
            select rowid from SpatialIndex
            where f_table_name = 'lines'
            and f_geometry_column = 'GEOMETRY'
            and search_frame = BuildCircleMBR("""+lon+""", """+lat+""", """+nearestPathThreshold.toDegreesAt(lat)+""")
          ) order by distance limit 1""",
          { row:Map[String, String] =>
            rv = Some(AndroidPath(m, List(row("osm_id").toInt), namePath(row), row.get("highway"), row("geom")))
            false
          }
        )
      }
      rv
    }
  }

  previous = None

}
