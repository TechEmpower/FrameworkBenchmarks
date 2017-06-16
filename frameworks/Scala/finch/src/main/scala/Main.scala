import com.twitter.io.Buf
import com.twitter.finagle.Http
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.Service
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.util.Await

import io.circe.Json
import io.finch._
import io.finch.circe._

object Main extends App {

  val helloWorld: Buf = Buf.Utf8("Hello, World!")

  val json: Endpoint[Json] = get("json") {
    Ok(Json.obj("message" -> Json.fromString("Hello, World!")))
  }

  val plaintext: Endpoint[Buf] = get("plaintext") {
    Ok(helloWorld)
  }

  val service: Service[Request, Response] =
    Bootstrap.configure(includeDateHeader = true, includeServerHeader = true)
      .serve[Application.Json](json)
      .serve[Text.Plain](plaintext)
      .toService

  Await.ready(Http.server
    .configured(Http.Netty3Impl)
    .withCompressionLevel(0)
    .withStatsReceiver(NullStatsReceiver)
    .withTracer(NullTracer)
    .serve(":9000", service)
  )
}
