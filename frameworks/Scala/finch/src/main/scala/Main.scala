import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

import com.twitter.io.Buf
import com.twitter.finagle.http.Response
import com.twitter.finagle.Http
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.util.Await

import io.circe.Json
import io.finch._
import io.finch.circe._

object Main extends App {

  val timeFormatter: DateTimeFormatter = 
    DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneOffset.UTC)

  val helloWorld: Buf = Buf.Utf8("Hello, World!")

  val json: Endpoint[Json] = get("json") {
    Ok(Json.obj("message" -> Json.fromString("Hello, World!")))
      .withHeader("Server" -> "Finch")
      .withHeader("Date" -> timeFormatter.format(Instant.now()))
  }

  // Downgrade a text/plain endpoint to `Endpoint[Response]` as per
  // https://github.com/finagle/finch/blob/master/docs/cookbook.md#serving-multiple-content-types
  val plaintext: Endpoint[Response] = get("plaintext") {
    val rep = Response()
    rep.content = helloWorld
    rep.contentType = "text/plain"
    rep.headerMap.set("Server", "Finch")
    rep.headerMap.set("Date", timeFormatter.format(Instant.now()))

    Ok(rep)
  }

  Await.ready(Http.server
    .withCompressionLevel(0)
    .withStatsReceiver(NullStatsReceiver)
    .withTracer(NullTracer)
    .serve(":9000", (json :+: plaintext).toServiceAs[Application.Json])
  )
}
