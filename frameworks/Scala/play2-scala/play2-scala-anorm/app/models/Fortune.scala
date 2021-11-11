package models

import javax.inject.{Inject, Singleton}

import anorm._
import anorm.SqlParser._
import play.api.db.Database
import scala.language.postfixOps

case class Fortune(id: Long, message: String)

@Singleton()
class FortunesDAO @Inject()(protected val db: Database) {

  private val simpleRowParser = {
    get[Long]("fortune.id") ~
    get[String]("fortune.message") map {
      case id~message => Fortune(id, message)
    }
  }

  def getAll: List[Fortune] = {
    db.withConnection { implicit connection =>
      SQL("SELECT * FROM Fortune").as(simpleRowParser *)
    }
  }
}