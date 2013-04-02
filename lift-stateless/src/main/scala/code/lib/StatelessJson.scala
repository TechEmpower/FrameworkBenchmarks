package code.lib

import _root_.net.liftweb._
import http._
import js._
import JsCmds._
import common._
import json._


/**
 * Respond to JSON requests in a stateless dispatch
 */
object StatelessJson {
  def init() {
    // register the JSON handler
    LiftRules.statelessDispatch.append{
      case r @ Req("json" :: Nil, _, _) => () => sayHello()
    }
  }

  def sayHello() = Full(JsonResponse(
    JE.JsObj("message" -> "Hello World!")
  ))
}
