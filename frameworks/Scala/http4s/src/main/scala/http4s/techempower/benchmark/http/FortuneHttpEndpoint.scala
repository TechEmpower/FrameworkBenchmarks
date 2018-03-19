package http4s.techempower.benchmark.http

import cats.Monad
import cats.effect.Effect
import cats.implicits._
import http4s.techempower.benchmark.model.Fortune
import http4s.techempower.benchmark.service.FortuneService
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Type`
import org.http4s.{HttpService, MediaType}

final class FortuneHttpEndpoint[F[_]: Effect](
    fortuneService: FortuneService[F]) {

  def service(implicit F: Monad[F]): HttpService[F] = {
    val dsl: Http4sDsl[F] = new Http4sDsl[F] {}
    import dsl._
    HttpService[F] {
      case GET -> Root / "fortune" =>
        for {
          fs <- fortuneService.selectFortune
          sfs = finalizeFortuneList(fs)
          template = fortuneTemplate(sfs)
          r <- Ok(template, `Content-Type`.apply(MediaType.`text/html`))
        } yield r
    }
  }

  @inline private[this] def finalizeFortuneList(
      fs: List[Fortune]): List[Fortune] =
    (Fortune(0, "Additional fortune added at request time.") :: fs)
      .sortBy(_.message)

  @inline private[this] def fortuneTemplate(fs: List[Fortune]): String =
    s"""<!DOCTYPE html>
      |<html>
      |<head><title>Fortunes</title></head>
      |<body>
      |<table>
      |    <tr><th>id</th><th>message</th></tr>
      |    ${fs.foldLeft("")((s, f) =>
         s + s"""<tr><td>${f.id}</td><td>${f.message}</td></tr>""")}
      |</table>
      |</body>
      |</html>
      |""".stripMargin
}
