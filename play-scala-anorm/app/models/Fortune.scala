package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps

case class Fortune(id: Long, message: String)

object Fortune {

  val simpleRowParser = {
    get[Long]("fortune.id") ~
    get[String]("fortune.message") map {
      case id~message => Fortune(id, message)
    }
  }

  def getAll(): List[Fortune] = {
    DB.withConnection { implicit connection =>
      SQL("SELECT * FROM Fortune").as(Fortune.simpleRowParser *)
    }
  }
}