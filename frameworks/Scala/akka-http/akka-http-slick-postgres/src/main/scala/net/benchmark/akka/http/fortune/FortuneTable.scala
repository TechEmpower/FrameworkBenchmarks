package net.benchmark.akka.http.fortune

import slick.jdbc.PostgresProfile.api._

object FortuneTable {

  val fortuneTableQuery: TableQuery[FortuneTable] = TableQuery[FortuneTable]

}

class FortuneTable(tag: Tag) extends Table[Fortune](tag, "Fortune") {

  def id = column[Int]("id", O.PrimaryKey)

  def message = column[String]("message")

  def * = (id, message).<>(Fortune.tupled, Fortune.unapply)

}
