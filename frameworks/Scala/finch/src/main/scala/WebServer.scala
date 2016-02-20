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

    case class Message(message: String)
    Ok(Message("Hello, World!").asJson)
  }

  val plaintext: Endpoint[String] = get("plaintext") {
    Ok("Hello, World!")
      .withContentType(Some("text/plain"))
  }

  val api = json :+: plaintext

  Await.ready(
    Http.serve(":9000", api.toService)
  )
}
