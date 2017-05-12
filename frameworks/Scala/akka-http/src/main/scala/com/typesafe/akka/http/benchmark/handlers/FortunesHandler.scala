package com.typesafe.akka.http.benchmark.handlers

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpCharsets._
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import com.typesafe.akka.http.benchmark.Infrastructure
import com.typesafe.akka.http.benchmark.Templating
import com.typesafe.akka.http.benchmark.datastore.DataStore
import org.fusesource.scalate.TemplateEngine

import scala.concurrent.Future
import scala.util.{Failure, Success}

trait FortunesHandler { _: Infrastructure with DataStore with Templating =>
  def fortunesEndpoint = get {
    path("fortunes") {
      onComplete(response) {
        case Success(record) => complete(record)
        case Failure(t) => failWith(t)
      }
    }
  }

  def response: Future[HttpResponse] = {
    getFortunes.map {
      fortunes =>
        val body = layout("/templates/fortunes.mustache", Map("fortunes" -> fortunes))
        HttpResponse(StatusCodes.OK, entity = HttpEntity(body).withContentType(`text/html`.withCharset(`UTF-8`)))
    }

  }
}
