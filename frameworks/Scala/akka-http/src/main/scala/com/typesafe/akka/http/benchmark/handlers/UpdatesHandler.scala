package com.typesafe.akka.http.benchmark.handlers

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpCharsets._
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.{ HttpEntity, HttpResponse, StatusCodes }
import akka.http.scaladsl.server.Directives._
import com.typesafe.akka.http.benchmark.Infrastructure
import com.typesafe.akka.http.benchmark.datastore.DataStore
import com.typesafe.akka.http.benchmark.entity.World
import com.typesafe.akka.http.benchmark.util.RandomGenerator
import spray.json.{ DefaultJsonProtocol, RootJsonFormat }

import scala.concurrent.Future
import scala.util.control.Exception._
import scala.util.{ Failure, Success }

trait UpdatesHandler { _: Infrastructure with DataStore with RandomGenerator =>
  import UpdatesHandler.Protocols._

  def updatesEndpoint = get {
    path("updates") {
      parameter('queries.?) { queries =>
        onComplete(response(queries)) {
          case Success(worlds) => complete(worlds)
          case Failure(t)      => failWith(t)
        }
      }
    }
  }

  private val catcher = catching(classOf[NumberFormatException]).withApply(t => 1)

  private def response(queries: Option[String]): Future[HttpResponse] = {
    val range = queries.map(i => catcher {
      i.toInt
    }).getOrElse(1).min(500).max(1)
    Future.sequence {
      (0 until range).toList.map {
        _ => nextRandomInt
      }.map {
        id => findOne(id)
      }
    }.map {
      worlds => worlds.map(_.copy(randomNumber = nextRandomInt))
    }.flatMap {
      worlds => Future.sequence(worlds.map(world => updateOne(world.id, world.randomNumber).map(_ => world)))
    }.map {
      worlds => HttpResponse(StatusCodes.OK, entity = HttpEntity(worlds.map(_.toResponse).toJson.toString()).withContentType(`application/json`))
    }
  }
}

object UpdatesHandler {

  object Protocols extends DefaultJsonProtocol {

    case class Response(id: Int, randomNumber: Int)

    implicit val responseFormat: RootJsonFormat[Response] = jsonFormat2(Response.apply)

    implicit val responseListFormat: RootJsonFormat[List[Response]] = listFormat[Response]

    implicit class ToResponse(record: World) {
      def toResponse = Response(record.id, record.randomNumber)
    }

  }

}