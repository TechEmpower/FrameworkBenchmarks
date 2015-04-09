package fi.markoa.tfb.spray_es

import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.Logger
import org.json4s.Extraction
import org.json4s.JsonAST.JInt
import org.slf4j.LoggerFactory
import spray.http.HttpHeaders.{`Content-Type`, `Content-Length`}
import spray.routing.HttpService

import akka.actor.Actor
import spray.http._
import MediaTypes._

import scala.util.{Try, Failure, Success}

import scala.concurrent.Future


class BenchmarkServiceActor extends Actor with BenchmarkService {
  def actorRefFactory = context
  def receive = runRoute(myRoute)
}

case class HelloMessage(message: String)
case class World(id: Int, randomNumber: Int)

trait BenchmarkService extends HttpService {
  import scala.concurrent._
  import org.json4s.jackson.JsonMethods._
  import spray.http._
  import spray.client.pipelining._
  import Boot.system.dispatcher
  implicit val formats = org.json4s.DefaultFormats
  val logger = Logger(LoggerFactory.getLogger(classOf[BenchmarkService]))
  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
  val config = ConfigFactory.load()
  val tfbDbHost = config.getString("tfb.db_host")
  val baseUrl = s"http://$tfbDbHost:9200/tfb/world"
  val MinWorldId = 1
  val MaxWorldId = 10000
  val MinQueries = 1
  val MaxQueries = 500

  val helloPlain = "Hello, World!"

  val myRoute =
    get {
      path("plaintext") { // Test type 6: Plaintext
        respondWithMediaType(`text/plain`) {
          complete(helloPlain)
        }
      } ~
      path("json") { // Test type 1: JSON serialization
        respondWithMediaType(`application/json`) {
          complete(compact(Extraction.decompose(HelloMessage(helloPlain))))
        }
      } ~
      path("db") { // Test type 2: Single database query
        respondWithMediaType(`application/json`) {
          onComplete(getRandomWorldFuture) {
            case Success(Right(r)) => complete(compact(Extraction.decompose(r)))
            case Success(Left(msg)) => complete(StatusCodes.InternalServerError, msg)
            case Failure(ex) => failWith(ex)
          }
        }
      } ~
      path("queries") { // Test type 3: Multiple database queries
        parameter("queries".?) { queries =>
          respondWithMediaType(`application/json`) {
            onComplete(getWorldQueries(queries)) {
              case Success(s) =>
                complete((s collect {
                  case Right(w) => compact(Extraction.decompose(w))
                  case Left(msg) => logger.error(msg)
                }) mkString("[", ",", "]"))
              case Failure(ex) => failWith(ex)
            }
          }
        }
      } ~
      path("updates") { // Test type 5: Database updates
        parameter("queries".?) { queries =>
          respondWithMediaType(`application/json`) {
            onComplete(getRandomInts(getQueriesParameter(queries), MinWorldId, MaxWorldId) zip getWorldQueries(queries)) {
              case Success((randoms, queryResults)) =>
                val results = (queryResults zip randoms).foldLeft((List[String](), List[String]())) { (acc, i) => i match {
                  case (Right(world), rand) =>
                    (compact(Extraction.decompose(world.copy(randomNumber = rand))) :: acc._1,
                      raw"""{ "index" : { "_id" : "${world.id}" } }
{ "randomNumber" : $rand }""" :: acc._2)
                  case _ => ("" :: acc._1, "" :: acc._2)
                  }
                }
                pipeline(Post(s"$baseUrl/_bulk", results._2.mkString("\n")))
                complete(results._1.mkString("[", ",", "]"))
              case Failure(ex) => failWith(ex)
            }
          }
        }
      }
    }

  def getRandomInts(count: Int, min: Int, max: Int) =
    Future { 1 to count map { i => ThreadLocalRandom.current.nextInt(min, max+1) } }

  def getWorldQueries(queries: Option[String]) =
    Future.sequence(1 to getQueriesParameter(queries) map (i => getRandomWorldFuture))

  def getQueriesParameter(queries: Option[String]) =
    (queries map (m => Try(m.toInt).map { i =>
      if (i < MinQueries) MinQueries
      else if (i > MaxQueries) MaxQueries
      else i
    }.getOrElse(MinQueries))).getOrElse(MinQueries)

  def getRandomWorldFuture = {
    (for {
      id <- Future { ThreadLocalRandom.current.nextInt(1, MaxWorldId+1) }
      res <- pipeline(Get(s"$baseUrl/$id"))
    } yield (id, res)) map { r =>
      if(r._2.status.intValue == 200) {
        parseOpt(r._2.entity.asString) map (c => c \ "_source" \ "randomNumber") match {
          case Some(JInt(rand)) => Right(World(r._1, rand.toInt))
          case _ => Left(s"parse error ${r._1}")
        }
      } else Left(s"error: id: ${r._1}, ${r._2.status}")
    }
  }

}
