package pekko.http.benchmark.handlers

import org.apache.pekko.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import org.apache.pekko.http.scaladsl.model.HttpCharsets._
import org.apache.pekko.http.scaladsl.model.MediaTypes._
import org.apache.pekko.http.scaladsl.model._
import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.server.Route
import pekko.http.benchmark.{Infrastructure, Templating}
import pekko.http.benchmark.datastore.DataStore
import pekko.http.benchmark.entity.Fortune

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
