package http4s.techempower.benchmark

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

import cats.effect._
import cats.implicits._
import fs2.{StreamApp, Stream}

import org.http4s._
import org.http4s.headers.`Content-Type`
import org.http4s.dsl._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.twirl._

import doobie.hikari.HikariTransactor
import doobie._
import doobie.implicits._

import java.util.concurrent.ThreadLocalRandom

import scala.concurrent.ExecutionContext.Implicits.global

case class Message(message: String)
case class World(id: Int, randomNumber: Int)
case class Fortune(id: Int, message: String)

// Extract queries parameter (with default and min/maxed)
object Queries {
  def unapply(params: Map[String,Seq[String]]): Option[Int] =
    Some(params.get("queries").getOrElse(Nil).headOption match {
      case None => 1
      case Some(x) => Math.max(1, Math.min(500, scala.util.Try(x.toInt).getOrElse(1)))
    })
}

object WebServer extends StreamApp[IO] with Http4sDsl[IO] {

  implicit val messageCodec: JsonValueCodec[Message] = JsonCodecMaker.make[Message](CodecMakerConfig())
  implicit val worldCodec: JsonValueCodec[World] = JsonCodecMaker.make[World](CodecMakerConfig())
  implicit val worldListCodec: JsonValueCodec[List[World]] = JsonCodecMaker.make[List[World]](CodecMakerConfig())
  implicit val fortuneCodec: JsonValueCodec[Fortune] = JsonCodecMaker.make[Fortune](CodecMakerConfig())


  implicit def jsonEncoder[T: JsonValueCodec]: EntityEncoder[IO, T] =
    EntityEncoder
      .byteArrayEncoder[IO]
      .contramap((data: T) => writeToArray(data))
      .withContentType(`Content-Type`(MediaType.`application/json`, Some(Charset.`UTF-8`)))

  def addHeaders(service: HttpService[IO]): HttpService[IO] =
    cats.data.Kleisli { req: Request[IO] =>
      service.run(req).map(_.putHeaders(Header("Server", req.serverAddr)))
    }

  def connectDatabase(host: String, poolSize: Int): IO[HikariTransactor[IO]] = {
    val driver = "org.postgresql.Driver"
    val url = s"jdbc:postgresql://$host/hello_world"
    val user = "benchmarkdbuser"
    val pass = "benchmarkdbpass"
    val maxPoolSize = poolSize
    val minIdle = poolSize
    for {
      xa <- HikariTransactor.newHikariTransactor[IO](driver, url, user, pass)
      _  <- xa.configure(ds => IO {
         ds.setMaximumPoolSize(maxPoolSize)
         ds.setMinimumIdle(minIdle)
      })
    } yield xa
  }

  // Provide a random number between 1 and 10000 (inclusive)
  val randomWorldId: IO[Int] = IO(ThreadLocalRandom.current.nextInt(1, 10001))

  // Update the randomNumber field with a random number
  def updateRandomNumber(world: World): IO[World] = {
    randomWorldId map { id =>
      world.copy(randomNumber = id)
    }
  }

  // Select a World object from the database by ID
  def selectWorld(xa: Transactor[IO], id: Int): IO[World] = {
    val query = sql"select id, randomNumber from World where id = $id".query[World]
    query.unique.transact(xa)
  }

  // Select a random World object from the database
  def selectRandomWorld(xa: Transactor[IO]): IO[World] = {
    randomWorldId flatMap { id =>
      selectWorld(xa, id)
    }
  }

  // Select a specified number of random World objects from the database
  def getWorlds(xa: Transactor[IO], numQueries: Int): IO[List[World]] =
    (0 until numQueries).toList.traverse(_ => selectRandomWorld(xa))

  // Update the randomNumber field with a new random number, for a list of World objects
  def getNewWorlds(worlds: List[World]): IO[List[World]] = {
    worlds.traverse(updateRandomNumber)
  }

  // Update the randomNumber column in the database for a specified set of World objects,
  // this uses a batch update SQL call.
  def updateWorlds(xa: Transactor[IO], newWorlds: List[World]): IO[Int] = {
    val sql = "update World set randomNumber = ? where id = ?"
    val update = Update[(Int, Int)](sql).updateMany(newWorlds.map(w => (w.randomNumber, w.id)))
    update.transact(xa)
  }

  // Retrieve all fortunes from the database
  def getFortunes(xa: Transactor[IO]): IO[List[Fortune]] = {
    val query = sql"select id, message from Fortune".query[Fortune]
    query.to[List].transact(xa)
  }

  // Add a new fortune to an existing list, and sort by message.
  def getSortedFortunes(old: List[Fortune]): List[Fortune] = {
    val newFortune = Fortune(0, "Additional fortune added at request time.")
    (newFortune :: old).sortBy(_.message)
  }

  // HTTP service definition
  def service(xa: Transactor[IO]) = HttpService[IO] {
    case GET -> Root / "json" =>
      Ok(Message("Hello, World!"))

    case GET -> Root / "db" =>
      Ok(selectRandomWorld(xa))

    case GET -> Root / "queries" :? Queries(numQueries) =>
      Ok(getWorlds(xa, numQueries))

    case GET -> Root / "fortunes" =>
      val page = for {
        oldFortunes <- getFortunes(xa)
        newFortunes = getSortedFortunes(oldFortunes)
      } yield html.index(newFortunes)
      Ok(page)

    case GET -> Root / "updates" :? Queries(numQueries) =>
      val updated = for {
        worlds <- getWorlds(xa, numQueries)
        newWorlds <- getNewWorlds(worlds)
        _ <- updateWorlds(xa, newWorlds)
      } yield newWorlds
      Ok(updated)

    case GET -> Root / "plaintext" =>
      Ok("Hello, World!")
  }

  // Given a fully constructed HttpService, start the server and wait for completion
  def startServer(service: HttpService[IO]) = {
    BlazeBuilder[IO]
      .bindHttp(8080, "0.0.0.0")
      .mountService(service, "/")
      .serve
  }

  // Entry point when starting service
  override def stream(args: List[String], requestShutdown: IO[Unit]) = {
    for {
      xa <- Stream.eval(connectDatabase(
        args.headOption.getOrElse("localhost"),
        sys.env.get("DB_POOL_SIZE").map(_.toInt).getOrElse(256)
      ))
      exitCode <- startServer(addHeaders(service(xa)))
    } yield exitCode
  }
}
