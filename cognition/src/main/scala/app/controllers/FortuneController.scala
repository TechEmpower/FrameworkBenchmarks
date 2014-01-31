package controllers

import scala.collection.mutable.ArrayBuffer

import org.cognition._
import conf.Authentication._
import models._
import views.fortunes._
import sqltyped._

class FortuneController extends Controller {

  get("fortunes") {
    withJDBC { _ => session =>
      implicit val conn = session.conn
      val buffer = new ArrayBuffer[Fortune]
      buffer += new Fortune(0, "Additional fortune added at request time.")
      sql("select id, message from fortune").apply
        .foreach(x => buffer += new Fortune(x.get("id"), x.get("message")))

      render.html(Index(buffer.sortBy(_.message)))
    }
  }

}

