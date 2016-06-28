package com.typesafe.akka.http.benchmark.handlers

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpCharsets._
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import com.typesafe.akka.http.benchmark.datastore.DataStore
import org.fusesource.scalate.TemplateEngine

import scala.concurrent.Future
import scala.util.{Failure, Success}

class FortunesHandler(components: {
  val system: ActorSystem
  val dataStore: DataStore
  val templateEngine: TemplateEngine
}) {

  import components.system.dispatcher

  val dataStore = components.dataStore
  val engine = components.templateEngine

  def endpoint = get {
    path("fortunes") {
      onComplete(response) {
        case Success(record) => complete(record)
        case Failure(t) => failWith(t)
      }
    }
  }

  def response: Future[HttpResponse] = {
    dataStore.getFortunes.map {
      fortunes =>
        val body = engine.layout("/templates/fortunes.mustache", Map("fortunes" -> fortunes))
        new HttpResponse(StatusCodes.OK, entity = HttpEntity(body).withContentType(`text/html`.withCharset(`UTF-8`)))
    }

  }
}
