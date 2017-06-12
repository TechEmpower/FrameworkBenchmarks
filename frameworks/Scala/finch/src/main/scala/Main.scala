import com.twitter.io.Buf
import com.twitter.finagle.Http
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
      .withHeader("Server" -> "Finch")
  }

  // We need to downgrade to Buf and override Content-Type header manually
  // to serve non-JSON payload out of the JSON service. This workaround
  // shouldn't be needed in Finch 1.0.
  val plaintext: Endpoint[Buf] = get("plaintext") {
    Ok(helloWorld)
      .withHeader("Server" -> "Finch")
      .withHeader("Content-Type" -> "text/plain")
  }

  Await.ready(Http.server
    .configured(Http.Netty3Impl)
    .withCompressionLevel(0)
    .withStatsReceiver(NullStatsReceiver)
    .withTracer(NullTracer)
    .serve(":9000", (json :+: plaintext).toServiceAs[Application.Json])
  )
}
