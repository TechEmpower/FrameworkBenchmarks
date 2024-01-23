package net.benchmark.akka.http.world

import slick.jdbc.PostgresProfile.api._

object WorldTable {

  val worldTableQuery: TableQuery[WorldTable] = TableQuery[WorldTable]

}

class WorldTable(tag: Tag) extends Table[World](tag, "World") {

  def id = column[Int]("id", O.PrimaryKey)

  def randomNumber = column[Int]("randomnumber")

  def * = (id, randomNumber).<>(World.tupled, World.unapply)
}
