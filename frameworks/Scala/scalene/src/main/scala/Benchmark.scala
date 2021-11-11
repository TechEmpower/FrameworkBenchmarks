package scalene.benchmark

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import scalene.actor.Pool
import scalene.routing._
import scalene.http.{Body, BodyFormatter, ContentType}
import scalene.sql._
import BasicConversions._

object Main extends App {

  Class.forName("org.postgresql.Driver");

  trait JsonMessage
  case class JsonRouteMessage(message: String) extends JsonMessage
  case class DBRouteMessage(id: Int, randomnumber: Int) extends JsonMessage
  case class MultiDBRouteMessage(items: Array[DBRouteMessage]) extends JsonMessage

  implicit val messageFormatter = new BodyFormatter[JsonMessage] {
    val mapper: ObjectMapper = new ObjectMapper().registerModule(DefaultScalaModule)
    def apply(msg: JsonMessage) = {
      val obj = msg match {
        case MultiDBRouteMessage(items) => items
        case other => other
      }
      Body(mapper.writeValueAsBytes(obj), Some(ContentType.`application/json`))
    }
  }

  val settings = Settings.basic(
    serverName = "scalene",
    port = 8080,
    server = ServerSettings.Default
  )

  
  implicit val pool = new Pool
  val worldClient = MiniSQL.client(
    "world-client",
    "jdbc:postgresql://tfb-database:5432/hello_world",
    "benchmarkdbuser",
    "benchmarkdbpass"
  )

  val random = new java.util.Random
  
  def randomWorld(session: MiniSQLSession): Option[DBRouteMessage] = {
    val stmt = session.prepared("SELECT id, randomnumber FROM world WHERE id = (?)")
    stmt.setInt(1, math.abs(random.nextInt) % 10000 + 1)
    val rs = stmt.executeQuery()
    if (rs.next()) {
      Some(DBRouteMessage(rs.getInt(1), rs.getInt(2)))
    } else {
      None
    }      
  }

  val dbRoute = GET / "db" to {_ =>
    worldClient.query{session =>
      randomWorld(session).map{_.ok}.getOrElse("N/A".notFound)
    }
  }

  val QueryNum = ![Int]
    .map{i => if (i < 1) 1 else if (i > 500) 500 else i}
    .recover{_ => 1}

  val multiRoute = GET / "queries" / QueryNum to {num =>
    worldClient.query{session =>
      MultiDBRouteMessage(Array.fill(num)(randomWorld(session).get)).ok
    }
  }

  val plainBody = Body.plain("Hello, World!")

  val routes = Routes(
    GET / "plaintext" to {_ => plainBody.ok},
    GET / "json"      to {_ => JsonRouteMessage("Hello, World!").ok},
    dbRoute,
    multiRoute
  )

  Routing.start(settings, routes)
}

