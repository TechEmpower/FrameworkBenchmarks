package net.benchmark.akka.http.fortune

class FortuneRoute(
    fr: FortuneRepository,
    sd: ExecutionContextExecutor,
    fd: ExecutionContextExecutor)(implicit val system: ActorSystem) {

  private val te = new TemplateEngine()
  private val fmat: ActorMaterializer = ActorMaterializer(
    Deciders.resumingMat("fmat"))

  private implicit lazy val fm: ToEntityMarshaller[Seq[Fortune]] = {
    val fortunesTemplate = te.load("/templates/fortunes.mustache")
    Marshaller.opaque { fortunes =>
      HttpEntity(
        contentType = `text/html`.withCharset(`UTF-8`),
        string = te.layout("", fortunesTemplate, Map("fortunes" -> fortunes))
      )
    }
  }

  private def source(
      p: DatabasePublisher[Fortune]): Source[Fortune, NotUsed] = {
    Source
      .fromPublisher(p)
      .prepend(
        Source.single(Fortune(0, "Additional fortune added at request time.")))
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def route(): Route = {
    path("fortunes") {
      complete(
        source(fr.all())
          .runWith(Sink.seq[Fortune])(fmat)
          .flatMap(s => Future.successful(s.sortBy(_.message)))(sd))
    }
  }

}
