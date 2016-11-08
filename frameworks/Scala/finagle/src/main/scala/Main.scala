import java.util.Date

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.twitter.finagle.{Service, SimpleFilter, Http}
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.finagle.http.{Request, Response, HttpMuxer}
import com.twitter.util.{Await, Future}
import com.twitter.io.Buf

object Main extends App {

  val mapper: ObjectMapper = new ObjectMapper().registerModule(DefaultScalaModule)

  val helloWorld: Buf = Buf.Utf8("Hello, World!")

  val muxer: HttpMuxer = new HttpMuxer()
    .withHandler("/json", Service.mk { req: Request =>
      val rep = Response()
      rep.content = Buf.ByteArray.Owned(mapper.writeValueAsBytes(Map("message" -> "Hello, World!")))
      rep.contentType = "application/json"

      Future.value(rep)
    })
    .withHandler("/plaintext", Service.mk { req: Request => 
      val rep = Response()
      rep.content = helloWorld
      rep.contentType = "text/plain"

      Future.value(rep)
    })

  val serverAndDate: SimpleFilter[Request, Response] = new SimpleFilter[Request, Response] {

    private[this] val addServerAndDate: Response => Response = { rep =>
        rep.server = "Finagle"
        rep.date = new Date()

        rep
    }

    def apply(req: Request, s: Service[Request, Response]): Future[Response] =
      s(req).map(addServerAndDate)
  }

  Await.ready(Http.server
    .withCompressionLevel(0)
    .withStatsReceiver(NullStatsReceiver)
    .withTracer(NullTracer)
    .serve(":8080", serverAndDate.andThen(muxer))
  )
}
