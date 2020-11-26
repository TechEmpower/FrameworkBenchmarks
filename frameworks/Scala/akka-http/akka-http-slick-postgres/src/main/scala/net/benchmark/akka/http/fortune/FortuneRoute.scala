package net.benchmark.akka.http.fortune

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes.`text/html`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{Sink, Source}
import net.benchmark.akka.http.util.Deciders
import org.fusesource.scalate.TemplateEngine
import slick.basic.DatabasePublisher
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

object FortuneRoute {

  private val ec1 = ExecutionContext.fromExecutorService(java.util.concurrent.Executors.newFixedThreadPool(7))

  private val te = new TemplateEngine()

  private val fortunesTemplate = FortuneRoute.te.load("/templates/fortunes.mustache")

  private val fm: ToEntityMarshaller[Seq[Fortune]] = {
    Marshaller.opaque { fortunes =>
      HttpEntity(
        contentType = `text/html`.withCharset(`UTF-8`),
        string = FortuneRoute.te.layout("", fortunesTemplate, Map("fortunes" -> fortunes))
      )
    }
  }

}

class FortuneRoute(fr: FortuneRepository, sd: ExecutionContextExecutor)(implicit val system: ActorSystem) {

  private implicit val fmar = FortuneRoute.fm

  private def source(p: DatabasePublisher[Fortune]): Source[Fortune, NotUsed] = {
    Source
      .fromPublisher(p)
      .prepend(Source.single(Fortune(0, "Additional fortune added at request time.")))
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def route(): Route = {
    path("fortunes") {
      complete(
        source(fr.all())
          .withAttributes(Deciders.resuming("fortunes"))
          .runWith(Sink.seq[Fortune])
          .flatMap(s => Future.successful(s.sortBy(_.message)))(FortuneRoute.ec1))
    }
  }

}
