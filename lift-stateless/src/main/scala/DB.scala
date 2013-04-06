package code.db

import code.Main

import org.apache.commons.dbcp.BasicDataSource
import javax.sql.DataSource
import Main.config
import scala.slick.driver.MySQLDriver.simple._

import Database.threadLocalSession


object DB {
  val dataSource: DataSource = {
    val ds = new BasicDataSource
    ds.setDriverClassName("com.mysql.jdbc.Driver")
    ds.setUsername("benchmarkdbuser")
    ds.setPassword("benchmarkdbpass")
    ds.setMaxActive(20);
    ds.setMaxIdle(10);
    ds.setInitialSize(10);
    ds.setValidationQuery("SELECT 1")
    ds.setUrl("jdbc:mysql://%s/%s?useUnicode=true&characterEncoding=UTF-8"
      format (config[String]("db.host"), config[String]("db.name")))
    ds
  }

  // test the data source validity
  dataSource.getConnection().close()

  // perform the migrations
  val database = Database.forDataSource(dataSource)

  val lastInsertIdFunction = SimpleFunction.nullary[Long]("LAST_INSERT_ID")
  def lastInsertId = Query(lastInsertIdFunction).firstOption


  /** runs the db code inside the transaction */
  def exec[A](fn: => A) =
    database withTransaction {
      fn
    }
}

  case class World(id: Option[Long], randomNumber: Long) {
    import net.liftweb.json._

    def toJson = JObject(List(
      JField("id", JInt(id.get)),
      JField("randomNumber", JInt(randomNumber))
    ))
  }

   object WorldTable extends Table[World]("World") {
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def randomNumber = column[Long]("randomNumber")

      def * = id.? ~ randomNumber <> (World, World.unapply _)
  }
