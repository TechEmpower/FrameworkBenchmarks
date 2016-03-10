import java.net.InetSocketAddress
import java.text.{DateFormat, SimpleDateFormat}
import java.util.Date

import io.finch._

import com.twitter.finagle.Service
import com.twitter.finagle.Http
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.util.Await

import io.circe._
import io.circe.generic.auto._
import io.circe.jawn._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

object WebServer extends App {
  private val dateFormat: DateFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z")

  import io.finch.circe._
  val json = get("json") {
    Ok(Json.obj("message" -> Json.string("Hello, World!")))
      .withHeader("Server" -> "finch")
      .withHeader("Date" -> dateFormat.format(new Date()))
  }

  val plaintext: Endpoint[String] = get("plaintext") {
    Ok("Hello, World!")
      .withHeader("Server" -> "finch")
      .withHeader("Date" -> dateFormat.format(new Date()))
      .withContentType(Some("text/plain"))
  }

  Await.ready(Http.server
    .withCompressionLevel(0)
    .withStatsReceiver(NullStatsReceiver)
    .withTracer(NullTracer)
    .serve(":9000", (json :+: plaintext).toService)
  )
}
