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

case class AndroidPath(val name:String, val classification:Option[String]) extends Path

object AndroidPath {
  def apply(db:Database, id:Int):Option[AndroidPath] = {
    var rv:Option[AndroidPath] = None
    db.exec(
      "select name, sub_type from ln_highway where id = "+id,
      { row:Map[String, String] =>
        rv = Some(new AndroidPath(row.get("name").getOrElse(""), row.get("sub_type")))
        false
      }
    )
    rv
  }
}

class AndroidIntersectionPosition(db:List[Database], id:Int, val perspective:Perspective, val lat:Double, val lon:Double) extends IntersectionPosition {

  lazy val paths = {
    var rv = List[Path]()
    db.map { d =>
      d.exec(
        "select osm_id, name, class from roads where node_from = "+id+" or node_to = "+id,
        { row:Map[String, String] =>
          rv ::= AndroidPath(d, row("osm_id").toInt).getOrElse {
            new AndroidPath(name, row.get("class"))
          }
          false
        }
      )
    }
    rv.distinct
  }

  override def hashCode = db.hashCode+id.hashCode

  override def equals(o:Any) = o match {
    case i:AndroidIntersectionPosition if(i.hashCode == hashCode) => true
    case _ => false
  }

}

class AndroidPerspective(db:List[Database], val lat:Double, val lon:Double, val direction:Option[Direction], val speed:Speed, val timestamp:Long, var previous:Option[Perspective]) extends Perspective {

  val nearestPoints = Nil

  lazy val nearestIntersectionCandidates = {
    var rv = List[IntersectionPosition]()
    db.map(_.exec(
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
        rv ::= new AndroidIntersectionPosition(db, row("id").toInt, this, row("lat").toDouble, row("lon").toDouble)
        false
      }
    ))
    rv
  }

  lazy val nearestPath:Option[Path] = {
    calcNearestPath.orElse {
      var rv:Option[Path] = None
      db.map(_.exec(
        """select Distance(geometry, MakePoint("""+lon+""", """+lat+""")) as distance, name, sub_type
        from ln_highway
        where ln_highway.rowid in (
          select rowid from SpatialIndex
          where f_table_name = 'ln_highway'
          and f_geometry_column = 'geometry'
          and search_frame = BuildCircleMBR("""+lon+""", """+lat+""", """+nearestPathThreshold.toDegreesAt(lat)+""")
        ) order by distance limit 1""",
        { row:Map[String, String] =>
          rv = Some(AndroidPath(row.get("name").getOrElse(""), row.get("sub_type")))
          false
        }
      ))
      rv
    }
  }

  previous = None

}
