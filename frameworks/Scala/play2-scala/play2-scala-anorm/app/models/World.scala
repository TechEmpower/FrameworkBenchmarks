package models

import javax.inject.{Inject, Singleton}

import anorm._
import anorm.SqlParser._
import java.sql.Connection
import play.api.db.Database
import play.api.libs.json._
import play.db.NamedDatabase

case class World(id: Id[Long], randomNumber: Long)

@Singleton()
class WorldDAO @Inject()(@NamedDatabase("hello_world") protected val db: Database) {
  /**
   * Parse a World from a ResultSet
   */
  private val simpleRowParser = {
    get[Long]("world.id") ~
    get[Long]("world.randomNumber") map {
      case id~randomNumber => World(Id(id), randomNumber)
    }
  }

  /**
   * Retrieve a World by id.
   */
  def findById(id: Long)(implicit connection: Connection): World = {
    SQL"SELECT * FROM World WHERE id = $id".as(simpleRowParser.single)
  }

  def updateRandom(world: World)(implicit connection: Connection) {
    SQL"UPDATE World SET randomNumber = ${world.randomNumber} WHERE id = ${world.id.get}".executeUpdate()
  }
}

object WorldJsonHelpers {
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
}