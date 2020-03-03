package com.typesafe.akka.http.benchmark.handlers

import akka.http.scaladsl.marshalling.{ Marshaller, ToEntityMarshaller }
import akka.http.scaladsl.model.HttpCharsets._
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.akka.http.benchmark.Infrastructure
import com.typesafe.akka.http.benchmark.Templating
import com.typesafe.akka.http.benchmark.datastore.DataStore
import com.typesafe.akka.http.benchmark.entity.Fortune

trait FortunesHandler { _: Infrastructure with DataStore with Templating =>

  def fortunesEndpoint: Route =
    get {
      path("fortunes") {
        complete(getFortunes)
      }
    }

  private implicit lazy val fortunesMarshaller: ToEntityMarshaller[Seq[Fortune]] = {
    val fortunesTemplate = templateEngine.load("/templates/fortunes.mustache")
    Marshaller.opaque { fortunes =>
      HttpEntity(
        contentType = `text/html`.withCharset(`UTF-8`),
        string = templateEngine.layout("", fortunesTemplate, Map("fortunes" -> fortunes))
      )
    }
  }
}
