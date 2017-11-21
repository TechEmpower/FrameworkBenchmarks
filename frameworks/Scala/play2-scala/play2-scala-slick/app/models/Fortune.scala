package models

import javax.inject.{Inject, Singleton}

import play.api.db.slick.{HasDatabaseConfigProvider, DatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.Future

case class Fortune(id: Long, message: String)

@Singleton()
class FortunesDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  private val Fortunes = TableQuery[FortunesTable]

  def getAll(): Future[Seq[Fortune]] = db.run(Fortunes.result)


  def findById(id: Long): Future[Option[Fortune]] = {
    db.run(Fortunes.filter(_.id === id).result.headOption)
  }

  private class FortunesTable(tag: Tag) extends Table[Fortune](tag, Some("hello_world"), "Fortune") {

    def id = column[Long]("id", O.PrimaryKey)
    def message = column[String]("message")
    def * = (id, message) <>(Fortune.tupled, Fortune.unapply)

  }
}
