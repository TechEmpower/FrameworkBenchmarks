package net.benchmark.akka.http.fortune

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes.`text/html`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import net.benchmark.akka.http.util.Deciders
import org.fusesource.scalate.TemplateEngine
import slick.basic.DatabasePublisher

import scala.concurrent.{ExecutionContextExecutor, Future}

class FortuneRoute(fr: FortuneRepository, sd: ExecutionContextExecutor, fd: ExecutionContextExecutor)(
    implicit val system: ActorSystem) {

  private val te = new TemplateEngine()
  private val fmat: ActorMaterializer = ActorMaterializer(Deciders.resumingMat("fmat"))

  private implicit lazy val fm: ToEntityMarshaller[Seq[Fortune]] = {
    val fortunesTemplate = te.load("/templates/fortunes.mustache")
    Marshaller.opaque { fortunes =>
      HttpEntity(
        contentType = `text/html`.withCharset(`UTF-8`),
        string = te.layout("", fortunesTemplate, Map("fortunes" -> fortunes))
      )
    }
  }

  private def source(p: DatabasePublisher[Fortune]): Source[Fortune, NotUsed] = {
    Source
      .fromPublisher(p)
      .prepend(Source.single(Fortune(0, "Additional fortune added at request time.")))
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def route(): Route = {
    path("fortunes") {
      withExecutionContext(fd) {
        complete(
          source(fr.all())
            .runWith(Sink.seq[Fortune])(fmat)
            .flatMap(s => Future.successful(s.sortBy(_.message)))(sd))
      }
    }
  }

}
