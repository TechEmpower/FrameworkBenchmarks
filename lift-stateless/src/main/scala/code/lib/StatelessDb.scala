package code.lib

import _root_.net.liftweb._
import http._
import js._
import JsCmds._
import common._
import json._
import java.util.concurrent.ThreadLocalRandom
import code.model._

import mapper.{By, By_<}

object StatelessDb {
  private val DB_ROWS = 10000
  private implicit val formats = net.liftweb.json.DefaultFormats

  case class JsonWorld(id: Long, randomNumber: Long) {
    def toJson = Extraction.decompose(this)
  }

  implicit def world2jsonWorld(w: World) = JsonWorld(w.id.get, w.randomNumber.get)

  def init() {
    LiftRules.statelessDispatch.append{
      case r @ Req("db" :: Nil, _, _) => () => singleDBQuery()
      case r @ Req("db" :: queries :: Nil, _ , _) => () => dbQuery(queries.toInt)
    }
  }

  def dbQuery(count: Int) : Box[LiftResponse] = {
    val random = ThreadLocalRandom.current()
    val rows = for(i <- (1 to count)) yield World.find(By(World.id, random.nextInt(DB_ROWS))).get
    Full(JsonResponse(JArray(rows.map(_.toJson).toList)))
  }

  def singleDBQuery() : Box[LiftResponse] = {
    val random = ThreadLocalRandom.current()
    val row = World.find(By(World.id, random.nextInt(DB_ROWS)))
    println("" + row)
    row match {
      case Full(r) => Full(JsonResponse(r.toJson))
      case _ => Empty
    }
  }
}
