package models

import play.api.Play.current
import play.api.db.slick.Config.driver.simple._
import play.api.db.slick.DB
import play.api.libs.json._
import play.api.libs.functional.syntax._

class Worlds(tag: Tag) extends Table[World](tag, "World") {
  def id = column[Int]("id", O.PrimaryKey)
  def randomNumber = column[Long]("randomNumber")
  def * = (id, randomNumber) <> ((World.apply _).tupled, World.unapply _)
}
class WorldsTableQuery extends TableQuery(new Worlds(_)){
  val byId = this.findBy(_.id)

  def findById(id: Int): Option[World] = DB.withSession { implicit session =>
      byId(id).firstOption
  }

  val updateQuery = Compiled{ (id: Column[Int]) => this.where(_.id === id) }
  def updateRandom(world: World) {
    DB.withSession { implicit session: Session =>
      updateQuery(world.id).update(world)
    }
  }  
}

case class World(id: Int, randomNumber: Long)

object World {
  implicit val toJson = new Writes[World] {
    def writes(w: World): JsValue = {
      Json.obj(
        "id" -> w.id,
        "randomNumber" -> w.randomNumber
      )
    }
  }
}