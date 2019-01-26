package net.benchmark.akka.http.world

import net.benchmark.akka.http.db.CustomPostgresProfile.api._

object WorldTable {

  val worldTableQuery: TableQuery[WorldTable] = TableQuery[WorldTable]

}

class WorldTable(tag: Tag) extends Table[World](tag, Some("hello_world"), "World") {

  def id = column[Int]("id", O.PrimaryKey)

  def randomNumber = column[Int]("randomNumber")

  def * = (id, randomNumber) <> (World.tupled, World.unapply)
}
