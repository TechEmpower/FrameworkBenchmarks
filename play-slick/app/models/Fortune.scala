package models

import play.api.Play.current
import play.api.db.slick.Config.driver.simple._
import play.api.db.slick.DB


class Fortunes extends Table[Fortune]("Fortune") {
  def id = column[Long]("id", O.PrimaryKey)
  def message = column[String]("message")
  def * = id ~ message <> (Fortune.apply _, Fortune.unapply _)

  val byId = createFinderBy(_.id)

  def getAll(): List[Fortune] = DB.withSession { implicit session =>
    Query(this).list
  }
}

case class Fortune(id: Long, message: String)
