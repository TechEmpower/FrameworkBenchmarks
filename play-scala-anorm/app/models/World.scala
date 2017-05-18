package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class World(id: Pk[Long], randomNumber: Long)

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
        get[Pk[Long]]("world.id") ~
        get[Long]("world.randomNumber") map {
            case id~randomNumber => World(id, randomNumber)
        }
    }

  /**
   * Retrieve a World by id.
   */
    def findById(id: Long): World = {
        DB.withConnection { implicit connection =>
            SQL("SELECT * FROM world WHERE id = {id}").on(
                "id" -> id
            ).as(World.simpleRowParser.single)
        }
    }
}