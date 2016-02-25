import java.net.InetSocketAddress

import io.finch._

import com.twitter.finagle.Service
import com.twitter.finagle.Http

import com.twitter.util.Await

import io.circe._
import io.circe.generic.auto._
import io.circe.jawn._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

object WebServer extends App {

  import io.finch.circe._
  val json = get("json") {
    Ok(Json.obj("message" -> Json.string("Hello, World!")))
  }

  val plaintext: Endpoint[String] = get("plaintext") {
    Ok("Hello, World!")
      .withContentType(Some("text/plain"))
  }

  Await.ready(
    Http.serve(":9000", (json :+: plaintext).toService)
  )
}
