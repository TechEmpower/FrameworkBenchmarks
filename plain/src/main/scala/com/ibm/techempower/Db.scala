package com.ibm.techempower

import java.util.concurrent.ThreadLocalRandom.{ current => random }

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.collection.mutable.MutableList

import com.ibm.plain.ignore
import com.ibm.plain.rest.{ Form, Resource }
import com.ibm.plain.json.{ Json => J }
import com.ibm.plain.jdbc.withConnection
import com.ibm.plain.jdbc.ConnectionHelper._

final class Db

    extends DbResource {

  Get { get(None) }

  Get { form: Form => get(Some(form)) }

}

final class Update

    extends DbResource {

  Get { form: Form => update(form) }

}

sealed abstract class DbResource

    extends Resource {

  @inline protected[this] final def get(form: Option[Form]): J = {
    val output = new MutableList[J]
    val q = form match { case None => 1 case Some(f) => queries(f) }
    withConnection(datasource) { implicit connection =>
      for (i <- 1 to q) { for (j <- selectsql << next ! asJson) { output += j } }
    }
    form match { case None => output.head case _ => J(output.toList) }
  }

  @inline protected[this] final def update(form: Form): J = {
    val input = new MutableList[World]
    val output = new MutableList[J]
    val q = queries(form)
    withConnection(datasource) { implicit connection =>
      for (i <- 1 to q) { for (j <- selectsql << next ! asTuple) { input += j } }
      input.foreach {
        case (id, _) =>
          val randomNumber = next
          updatesql << randomNumber << id ++;
          output += asJson(id, randomNumber)
      }
      ignore(updatesql ++!)
    }
    J(output.toList)
  }

  @inline private[this] final def queries(form: Form) = try {
    form.get("queries").get.head.toInt match {
      case q if 1 > q => 1
      case q if 500 < q => 500
      case q => q
    }
  } catch {
    case _: Throwable => 1
  }

  @inline private[this] final def asJson = (r: RichResultSet) => J(Map("id" -> r.nextInt, "randomNumber" -> r.nextInt))

  @inline private[this] final def asJson(id: Int, randomNumber: Int) = J(Map("id" -> id, "randomNumber" -> randomNumber))

  @inline private[this] final def asTuple = (r: RichResultSet) => (r.nextInt, r.nextInt)

  @inline private[this] final def next = random.nextInt(1, 10001)

  private[this] final type World = (Int, Int)

  private[this] final val selectsql = "select id, randomNumber from World where id = ?"

  private[this] final val updatesql = "update World set randomNumber = ? where id = ?"

  private[this] final val datasource = "MysqlBenchmark"

}
