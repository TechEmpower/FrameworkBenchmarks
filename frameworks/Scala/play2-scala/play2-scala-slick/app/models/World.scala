package models

import play.api.libs.json._

import scala.concurrent.Future

import javax.inject.{Singleton, Inject}
import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfigProvider
import slick.jdbc.JdbcProfile
import scala.concurrent.ExecutionContext

@Singleton()
class WorldDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) (implicit ec: ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  private val Worlds = TableQuery[WorldsTable]

  def all(): Future[Seq[World]] = db.run(Worlds.result)

  def insert(world: World): Future[Unit] = db.run(Worlds += world).map { _ => () }

  private class WorldsTable(tag: Tag) extends Table[World](tag, Some("hello_world"), "World") {

    def id = column[Int]("id", O.PrimaryKey)
    def randomNumber = column[Long]("randomNumber")

    def * = (id, randomNumber) <> (World.tupled, World.unapply)
  }

  def findById(id: Int): Future[World] = {
    db.run(Worlds.filter(_.id === id).result.head)
  }

  def updateRandom(world: World) = {
    db.run(Worlds.filter(_.id === world.id).map(_.randomNumber).update(world.randomNumber))
  }
}


case class World(id: Int, randomNumber: Long)

object WorldJsonHelpers {
  implicit val toJson = new Writes[World] {
    def writes(w: World): JsValue = {
      Json.obj(
        "id" -> w.id,
        "randomNumber" -> w.randomNumber
      )
    }
  }
}
