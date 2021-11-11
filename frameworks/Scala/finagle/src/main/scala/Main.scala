import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.twitter.finagle.{Service, SimpleFilter, Http}
import com.twitter.finagle.stack.nilStack
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
      rep.headerMap.setUnsafe("Content-Type", "application/json")

      Future.value(rep)
    })
    .withHandler("/plaintext", Service.mk { req: Request => 
      val rep = Response()
      rep.content = helloWorld
      rep.headerMap.setUnsafe("Content-Type", "text/plain")

      Future.value(rep)
    })

  val serverAndDate: SimpleFilter[Request, Response] =
    new SimpleFilter[Request, Response] with (Response => Response) {

    def apply(rep: Response): Response = {
      rep.headerMap.setUnsafe("Server", "Finagle")
      rep.headerMap.setUnsafe("Date", currentTime())

      rep
    }

    def apply(req: Request, s: Service[Request, Response]): Future[Response] =
      s(req).map(this)
  }

  Await.ready(Http.server
    .withCompressionLevel(0)
    .withStack(nilStack[Request, Response])
    .serve(":8080", serverAndDate.andThen(muxer))
  )
}
