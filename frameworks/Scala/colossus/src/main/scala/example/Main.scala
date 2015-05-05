package example

import java.util.Date
import java.text.SimpleDateFormat

import colossus._
import service._
import protocols.http._
import UrlParsing._
import HttpMethod._

import net.liftweb.json._
import JsonDSL._

object Main extends App {

  implicit val io_system = IOSystem()

  Service.become[Http]("sample", 9007) {
    case request @ Get on Root / "json" => {
      val json = ("message" -> "Hello, World!")
      val sdf = new SimpleDateFormat("EEE, MMM d yyyy HH:MM:ss z")
      val v = request.ok(compact(render(json)))
        .withHeader("Content-Type", "application/json")
        .withHeader("Server", "Colossus")
        .withHeader("Date", sdf.format(new Date()))
      Callback.successful(v)
    }
    case request @ Get on Root / "plaintext" => {
      val sdf = new SimpleDateFormat("EEE, MMM d yyyy HH:MM:ss z")
      val res = request.ok("Hello, World!")
        .withHeader("Content-Type", "text/plain")
        .withHeader("Server", "Colossus")
        .withHeader("Date", sdf.format(new Date()))
      Callback.successful(res)
    }
  }
}