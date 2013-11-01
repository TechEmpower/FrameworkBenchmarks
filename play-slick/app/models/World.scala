package models

import play.api.Play.current
import play.api.db.slick.Config.driver.simple._
import play.api.db.slick.DB
import play.api.libs.json._
import play.api.libs.functional.syntax._

class Worlds extends Table[World]("World") {
  def id = column[Int]("id", O.PrimaryKey)
  def randomNumber = column[Long]("randomNumber")
  def * = id ~ randomNumber <> (World.apply _, World.unapply _)

  val byId = createFinderBy(_.id)

  def findById(id: Int): Option[World] = DB.withSession { implicit session =>
      byId(id).firstOption
  }

  def updateRandom(world: World) {
    DB.withSession { implicit session: Session =>
      this.where(_.id === world.id.bind).update(world)
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