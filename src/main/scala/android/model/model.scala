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

class AndroidRelativePosition(val perspective:Perspective, val lat:Double, val lon:Double) extends RelativePosition {

  val name = "Unnamed"

}

case class AndroidPath(db:Database, val name:String, val classification:Option[String], geom:String) extends Path {

  def crosses_?(p:Position) = {
    var rv = false
    db.exec(
      "select crosses(fromWKT('"+geom+"'), makePoint("+p.lon+", "+p.lat+")) as crosses",
      { row:Map[String, String] =>
        if(row("crosses").toInt == 1)
          rv = true
        false
      }
    )
    rv
  }

}

object AndroidPath {
  def apply(db:Database, id:Int):Option[AndroidPath] = {
    var rv:Option[AndroidPath] = None
    db.exec(
      "select name, sub_type, asWKT(geometry) as geom from ln_highway where id = "+id,
      { row:Map[String, String] =>
        rv = Some(new AndroidPath(db, row.get("name").getOrElse(""), row.get("sub_type"), row("geom")))
        false
      }
    )
    rv
  }
}

class AndroidIntersectionPosition(db:Database, id:Int, val perspective:Perspective, val lat:Double, val lon:Double) extends IntersectionPosition {

  lazy val paths = {
    var rv = List[Path]()
    db.exec(
      "select osm_id, name, class, asWKT(geometry) as geom from roads where node_from = "+id+" or node_to = "+id,
      { row:Map[String, String] =>
        rv ::= AndroidPath(db, row("osm_id").toInt).getOrElse {
          new AndroidPath(db, name, row.get("class"), row("geom"))
        }
        false
      }
    )
    rv.distinct
  }

  lazy val neighbors = {
    var ids = List[Int]()
    db.exec(
      "select osm_id, node_from, node_to from roads where node_from = "+id+" or node_to = "+id,
      { row:Map[String, String] =>
        val from = row("node_from").toInt
        val to = row("node_to").toInt
        ids ::= (if(from == id) to else from)
        false
      }
    )
    ids.map { id =>
      AndroidIntersectionPosition(db, id, perspective)
    }.filterNot(_ == None).map(_.get)
  }

  override def hashCode = db.hashCode+id.hashCode

  override def equals(o:Any) = o match {
    case i:AndroidIntersectionPosition if(i.hashCode == hashCode) => true
    case _ => false
  }

}

object AndroidIntersectionPosition {
  def apply(db:Database, id:Int, perspective:Perspective):Option[AndroidIntersectionPosition] = {
    var rv:Option[AndroidIntersectionPosition] = None
    db.exec(
      "select X(geometry) as lon, Y(geometry) as lat from roads_nodes where node_id = "+id,
      { row:Map[String, String] =>
        rv = Some(new AndroidIntersectionPosition(db, id, perspective, row("lat").toDouble, row("lon").toDouble))
        false
      }
    )
    rv
  }
}

class AndroidPerspective(db:List[Database], val lat:Double, val lon:Double, val direction:Option[Direction], val speed:Speed, val timestamp:Long, var previous:Option[Perspective]) extends Perspective {

  val nearestPoints = Nil

  lazy val nearestIntersectionCandidates = {
    var rv = List[IntersectionPosition]()
    db.map { d =>
      d.exec(
        """select Distance(geometry, MakePoint("""+lon+""", """+lat+""")) as distance,
        X(geometry) as lon,
        Y(geometry) as lat, node_id as id
        from roads_nodes
        where roads_nodes.rowid in (
          select rowid from SpatialIndex
          where f_table_name = 'roads_nodes'
          and f_geometry_column = 'geometry'
          and search_frame = BuildCircleMBR("""+lon+""", """+lat+""", """+closeProximityDegrees+""")
        ) order by distance""",
        { row:Map[String, String] =>
          rv ::= new AndroidIntersectionPosition(d, row("id").toInt, this, row("lat").toDouble, row("lon").toDouble)
          false
        }
      )
    }
    
    rv
  }

  lazy val nearestPath:Option[Path] = {
    calcNearestPath.orElse {
      var rv:Option[Path] = None
      db.map { d =>
        d.exec(
          """select Distance(geometry, MakePoint("""+lon+""", """+lat+""")) as distance, name, sub_type, asWKT(geometry) as geom
          from ln_highway
          where ln_highway.rowid in (
            select rowid from SpatialIndex
            where f_table_name = 'ln_highway'
            and f_geometry_column = 'geometry'
            and search_frame = BuildCircleMBR("""+lon+""", """+lat+""", """+nearestPathThreshold.toDegreesAt(lat)+""")
          ) order by distance limit 1""",
          { row:Map[String, String] =>
            rv = Some(AndroidPath(d, row.get("name").getOrElse(""), row.get("sub_type"), row("geom")))
            false
          }
        )
      }
      rv
    }
  }

  previous = None

}
