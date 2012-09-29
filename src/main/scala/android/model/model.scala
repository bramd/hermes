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

case class AndroidPath(db:Database, ids:List[Int], val name:String, val classification:Option[String], geom:String) extends Path {

  private[model] def joinWith(other:AndroidPath) = {
    var rv:AndroidPath = null
    db.exec(
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
    Log.d("hermescheck40", "select crosses(geomFromText('"+geom+"'), BuildCircleMBR("+other.lon+", "+other.lat+", 0.001)) as crosses")
    db.exec(
      "select crosses(geomFromText('"+geom+"'), BuildCircleMBR("+other.lon+", "+other.lat+", 0.001)) as crosses",
      { row:Map[String, String] =>
        Log.d("hermescheck40", "Got "+row)
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
        rv = Some(new AndroidPath(db, List(id), row.get("name").orElse(row.get("sub_type")).getOrElse(""), row.get("sub_type"), row("geom")))
        false
      }
    )
    rv
  }
}

class AndroidIntersectionPosition(db:Database, id:Int, val perspective:Perspective, val lat:Double, val lon:Double) extends IntersectionPosition {

  val paths = {
    var rv = List[AndroidPath]()
    db.exec(
      "select osm_id, name, class, asWKT(geometry) as geom from roads where node_from = "+id+" or node_to = "+id,
      { row:Map[String, String] =>
        val path = AndroidPath(db, row("osm_id").toInt).getOrElse {
          new AndroidPath(db, List(row("osm_id").toInt), row.get("name").orElse(row.get("class")).getOrElse(""), row.get("class"), row("geom"))
        }
        rv.find(_.name == path.name).headOption.map { same =>
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

case class AndroidPointOfInterest(val perspective:Perspective, val name:String, val classification:Option[String], val lat:Double, val lon:Double) extends PointOfInterest {
  override val toString = name+": "+distanceTo(perspective)+perspective.bearingTo(this).map(" "+_).getOrElse("")
}


class AndroidPerspective(db:List[Database], val lat:Double, val lon:Double, val direction:Option[Direction], val speed:Speed, val timestamp:Long, var previous:Option[Perspective]) extends Perspective {

  def nearestPoints(limit:Int = 10, skip:Int = 0) = {
    var rv = List[PointOfInterest]()
    db.map(_.exec(
      """select name, sub_type,
      X(PointOnSurface(geometry)) as lon, Y(PointOnSurface(geometry)) as lat,
      Distance(geometry, MakePoint("""+lon+""", """+lat+""")) as distance
      from pg_amenity
      where rowid in (
        select rowid from SpatialIndex
        where f_table_name = 'pg_amenity'
        and f_geometry_column = 'geometry'
        and search_frame = BuildCircleMBR("""+lon+""", """+lat+""", 0.1))
      order by distance limit """+limit,
      { row:Map[String, String] =>
        val cls = row.get("sub_type").flatMap { c =>
          if(c.isEmpty)
            None
          else Some(c.replace("_", " "))
        }
        val n = row.get("name").orElse(cls).getOrElse("Unnamed")
        rv ::= AndroidPointOfInterest(this, n, cls, row("lat").toDouble, row("lon").toDouble)
        false
      }
    ))
    rv.sortBy(distanceTo(_))
  }

  val nearestIntersectionCandidates = {
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
          and search_frame = BuildCircleMBR("""+lon+""", """+lat+""", """+nearestIntersectionDistance.toDegreesAt(lat)+""")
        ) order by distance""",
        { row:Map[String, String] =>
          rv ::= new AndroidIntersectionPosition(d, row("id").toInt, this, row("lat").toDouble, row("lon").toDouble)
          false
        }
      )
    }
    
    rv
  }

  val nearestPath:Option[Path] = {
    calcNearestPath.orElse {
      var rv:Option[Path] = None
      db.map { d =>
        d.exec(
          """select id, Distance(geometry, MakePoint("""+lon+""", """+lat+""")) as distance, name, sub_type, asWKT(geometry) as geom
          from ln_highway
          where ln_highway.rowid in (
            select rowid from SpatialIndex
            where f_table_name = 'ln_highway'
            and f_geometry_column = 'geometry'
            and search_frame = BuildCircleMBR("""+lon+""", """+lat+""", """+nearestPathThreshold.toDegreesAt(lat)+""")
          ) order by distance limit 1""",
          { row:Map[String, String] =>
            rv = Some(AndroidPath(d, List(row("id").toInt), row.get("name").getOrElse(""), row.get("sub_type"), row("geom")))
            false
          }
        )
      }
      rv
    }
  }

  previous = None

}
