package http4s.techempower.benchmark

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.server._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.twirl._
import headers._

import doobie._
import doobie.implicits._
import doobie.hikari.HikariTransactor

import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._

import cats.data.{Kleisli, ValidatedNel}
import cats.effect.IO
import cats.implicits._

import fs2.{StreamApp, Stream}

import java.util.concurrent.ThreadLocalRandom
import scala.concurrent.ExecutionContext.Implicits.global

case class World(id: Int, randomNumber: Int)

case class Fortune(id: Int, message: String)

object Middleware {
  def addHeaders(service: HttpService[IO]): HttpService[IO] =
    Kleisli { req: Request[IO] =>
      service.run(req).map { resp =>
        resp.putHeaders(Header("Server", req.serverAddr))
      }
    }
}

object Queries extends OptionalValidatingQueryParamDecoderMatcher[Int]("queries") {
  def clampQueries(numQueries: Option[ValidatedNel[ParseFailure, Int]]): Int = {
    numQueries.fold(1)(_.fold(
      errors => 1,
      queries => {
        if (queries < 1)
          1
        else if (queries > 500)
          500
        else
          queries
      }
    ))
  }
}

object WebServer extends StreamApp[IO] {

  def transactor(host: String): Stream[IO, Transactor[IO]] = Stream.eval {
    val driver = "org.postgresql.Driver"
    val url = s"jdbc:postgresql://$host/hello_world"
    val user = "benchmarkdbuser"
    val pass = "benchmarkdbpass"
    val maxPoolSize = 256
    val minIdle = 256

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
  def updateRandomNumber(world: World): IO[World] =
    randomWorldId map { id =>
      world.copy(randomNumber = id)
    }

  // Select a World object from the database by ID
  def selectWorld(xa: Transactor[IO], id: Int): IO[World] = {
    val query = sql"select id, randomNumber from World where id = $id".query[World]
    query.unique.transact(xa)
  }

  // Select a random World object from the database
  def selectRandomWorld(xa: Transactor[IO]): IO[World] =
    randomWorldId.flatMap(selectWorld(xa, _))

  // Select a specified number of random World objects from the database
  def getWorlds(xa: Transactor[IO], numQueries: Int): IO[List[World]] =
    List.fill(numQueries)(selectRandomWorld(xa)).parSequence

  // Update the randomNumber field with a new random number, for a list of World objects
  def getNewWorlds(worlds: List[World]): IO[List[World]] =
    worlds.traverse(updateRandomNumber)

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
      Ok(Json.obj("message" -> Json.fromString("Hello, World!")))

    case GET -> Root / "db" =>
      Ok(selectRandomWorld(xa).map(_.asJson))

    case GET -> Root / "queries" :? Queries(rawQueries) =>
      val numQueries = Queries.clampQueries(rawQueries)
      Ok(getWorlds(xa, numQueries).map(_.asJson))

    case GET -> Root / "fortunes" =>
      val page = for {
        oldFortunes <- getFortunes(xa)
        newFortunes = getSortedFortunes(oldFortunes)
      } yield html.index(newFortunes)

      Ok(page)

    case GET -> Root / "updates" :? Queries(rawQueries) =>
      val numQueries = Queries.clampQueries(rawQueries)

      for {
        worlds    <- getWorlds(xa, numQueries)
        newWorlds <- getNewWorlds(worlds)
        _         <- updateWorlds(xa, newWorlds)
        resp      <- Ok(newWorlds.asJson)
      } yield resp

    case GET -> Root / "plaintext" =>
      Ok("Hello, World!", `Content-Type`(MediaType.`text/plain`))
  }

  def stream(args: List[String], requestShutdown: IO[Unit]) =
    transactor(args.head).flatMap { xa =>
      BlazeBuilder[IO]
        .bindHttp(8080, "0.0.0.0")
        .mountService(Middleware.addHeaders(service(xa)))
        .serve
    }
}
