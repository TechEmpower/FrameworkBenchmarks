package ba.sake.sharaf.benchmark

import java.util.concurrent.ThreadLocalRandom
import scala.collection.mutable
import ba.sake.squery.{*, given}
import scala.collection.decorators.*

class DAO {
  private val ds = com.zaxxer.hikari.HikariDataSource()
  ds.setJdbcUrl("jdbc:postgresql://tfb-database:5432/hello_world")
  ds.setUsername("benchmarkdbuser")
  ds.setPassword("benchmarkdbpass")
  ds.setMaximumPoolSize(Runtime.getRuntime().availableProcessors() * 2 + 1)
  private val squeryContext = SqueryContext(ds)

  def getRandomWorld(): WorldRow = squeryContext.run {
    sql"SELECT id, randomnumber FROM world WHERE id = ${getRandomRowId()}"
      .readRow()
  }

  def getRandomWorlds(queriesCount: Int): Seq[WorldRow] = squeryContext.run {
    val buffer = new mutable.ArrayBuffer[WorldRow](queriesCount)
    for i <- 0 until queriesCount do
      buffer += sql"SELECT id, randomnumber FROM world WHERE id = ${getRandomRowId()}"
        .readRow()
    buffer.toSeq
  }

  def updateWorlds(rows: Seq[WorldRow]): Unit = squeryContext.run {
    val values = rows
      .map(row => sql"(${row.id}, ${row.randomnumber})")
      .intersperse(sql",")
      .reduce(_ ++ _)
    sql"""
    UPDATE world as w
    SET randomnumber = c.randomnumber
    FROM (VALUES ${values}) AS c(id, randomnumber)
    WHERE w.id = c.id
    """.update()
  }

  def getFortunes(): Seq[FortuneRow] = squeryContext.run {
    sql"SELECT id, message FROM fortune".readRows()
  }

  def getRandomRowId(): Int =
    return 1 + ThreadLocalRandom.current().nextInt(10000)

}

case class WorldRow(id: Int, randomnumber: Int) derives SqlReadRow
case class FortuneRow(id: Int, message: String) derives SqlReadRow
