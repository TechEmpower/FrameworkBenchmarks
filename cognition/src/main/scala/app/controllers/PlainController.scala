package controllers

import org.cognition._

class PlainController extends Controller {

  get("/plaintext")(_ => render.plain("Hello, World!"))

}
