package hello

import org.scalatra.ScalatraServlet
import java.lang.String

class JsonController extends ScalatraServlet with JsonSetup {

  case class Message(message: String)

  get("/") {
    Message("Hello, World!")
  }
}