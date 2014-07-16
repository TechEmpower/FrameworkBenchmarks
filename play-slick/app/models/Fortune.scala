package models

import play.api.Play.current
import play.api.db.slick.Config.driver.simple._
import play.api.db.slick.DB


class Fortunes(tag: Tag) extends Table[Fortune](tag, "Fortune") {
  def id = column[Long]("id", O.PrimaryKey)
  def message = column[String]("message")
  def * = (id, message) <> (Fortune.tupled, Fortune.unapply _)
}
class FortunesTableQuery extends TableQuery(new Fortunes(_)){
  val byId = this.findBy(_.id)
  val all = Compiled{this:Query[Fortunes,Fortune]}
  def getAll(): List[Fortune] = DB.withSession { implicit session =>
    all.list
  }
}

case class Fortune(id: Long, message: String)
