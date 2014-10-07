package models

import anorm._
import anorm.SqlParser._
import java.sql.Connection
import play.api.db._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.Play.current

case class World(id: Id[Long], randomNumber: Long)

object World {
    /**
    * Convert a World to Json object
    */
  implicit val toJson = new Writes[World] {
    def writes(w: World): JsValue = {
      Json.obj(
        "id" -> w.id.get,
        "randomNumber" -> w.randomNumber
      )
    }
  }

  /**
   * Parse a World from a ResultSet
   */
  val simpleRowParser = {
    get[Long]("world.id") ~
    get[Long]("world.randomNumber") map {
      case id~randomNumber => World(Id(id), randomNumber)
    }
  }

  /**
   * Retrieve a World by id.
   */
  def findById(id: Long)(implicit connection: Connection): World = {
    SQL"SELECT * FROM World WHERE id = ${id}".as(World.simpleRowParser.single)
  }

  def updateRandom(world: World)(implicit connection: Connection) {
    SQL"UPDATE World SET randomNumber = ${world.randomNumber} WHERE id = ${world.id.get}".executeUpdate()
  }
}