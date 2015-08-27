import java.net.InetSocketAddress

import io.finch._
import io.finch.route._
import io.finch.response._

import com.twitter.finagle.Service
import com.twitter.finagle.Httpx
import com.twitter.finagle.httpx.{Response, Request}

import com.twitter.util.Await

import io.circe._
import io.circe.generic.auto._
import io.circe.jawn._
import io.circe.syntax._
import io.finch.response.EncodeResponse
import io.circe.{Decoder, Encoder, Json}

object WebServer extends App {

  val json = get("json") {
    import io.finch.circe._

    case class Message(message: String)
    Ok(Message("Hello, World!").asJson)
  }

  val plaintext = get("plaintext") {
    "Hello, World!"
  }

  val api: Service[Request, Response] = (json :+: plaintext).toService

  Await.ready(
    Httpx.serve("localhost:9000", api)
  )
}
