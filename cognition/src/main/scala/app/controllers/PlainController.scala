package controllers

import org.cognition._

class PlainController extends Controller {

  get("/plaintext")(_ => response)

  private[this] final val response = render.plain("Hello, World!")

}
