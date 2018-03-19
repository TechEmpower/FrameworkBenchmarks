package http4s.techempower.benchmark

import cats.effect.{Effect, IO}
import fs2.{Scheduler, StreamApp}
import modules.ApiModule
import org.http4s.client.blaze.Http1Client
import org.http4s.server.blaze.BlazeBuilder
import monix.execution.Scheduler.Implicits.global

object Main extends Server[IO]

class Server[F[_]: Effect] extends StreamApp[F] {
  import config._

  override def stream(
      args: List[String],
      requestShutdown: F[Unit]): fs2.Stream[F, StreamApp.ExitCode] =
    Scheduler(corePoolSize = conf.corePoolSize) flatMap { implicit scheduler =>
      for {
        client <- Http1Client.stream[F]()
        ctx = new ApiModule[F]
        init <- BlazeBuilder[F]
          .bindHttp(conf.port, conf.host)
          .mountService(ctx.api, conf.apiRoot)
          .serve
      } yield init
    }
}
