package code.lib

import net.liftweb._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import net.liftweb.common._

import js._
import json._
import JsCmds._

import code.db._

import java.util.concurrent.ThreadLocalRandom

import scala.slick.driver.MySQLDriver.simple._
import Database.threadLocalSession

object JsonBenchmark {
  def init() =
    LiftRules.statelessDispatch.append{
            case r @ Req("json" :: Nil, _, _) => () => sayHello()
    }

  def sayHello() = Full(JsonResponse(
    JE.JsObj("message" -> "Hello World!")
  ))
}

object DbBenchmark {
  private val DB_ROWS = 10000


  def init() =
    LiftRules.statelessDispatch.append{
            case r @ Req("db" :: Nil, _, _) => () => singleQuery()
            case r @ Req("db" :: queries :: Nil, _ , _) => () => customQuery(queries.toInt)
    }

  def customQuery(count: Int) : Box[LiftResponse] = DB.exec {
    val tlc = ThreadLocalRandom.current()
    val randoms = for(i <- (1 to count)) yield tlc.nextLong(DB_ROWS)
    val result = (for {
      w <- WorldTable
      if w.id inSetBind randoms
    } yield w).list

    Full(JsonResponse(JArray(result.map(_.toJson))))
  }

  def singleQuery() : Box[LiftResponse] = DB.exec {
    val tlc = ThreadLocalRandom.current()
    val random = tlc.nextLong(DB_ROWS)

    val result  = (for {
      w <- WorldTable if w.id === random
    } yield w).list


    Full(JsonResponse(result(0).toJson))
  }

}
