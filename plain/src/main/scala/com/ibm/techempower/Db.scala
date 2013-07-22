package com.ibm.techempower

import java.util.concurrent.ThreadLocalRandom.{ current => random }

import scala.language.implicitConversions
import scala.language.postfixOps

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
    var list: List[J] = Nil
    val q = form match { case None => 1 case Some(f) => queries(f) }
    withConnection(datasource) {
      implicit connection => for (i <- 1 to q) { for (j <- selectsql << next <<! asJson) { list = j :: list } }
    }
    form match { case None => list.head case _ => J(list) }
  }

  @inline protected[this] final def update(form: Form): J = {
    var list: List[J] = Nil
    val q = queries(form)
    withConnection(datasource) {
      implicit connection =>
        for (i <- 1 to q) {
          val id = next
          val randomNumber = next
          updatesql << randomNumber << id <<!!;
          list = asJson(id, randomNumber) :: list
        }
    }
    J(list)
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

  @inline private[this] final def asJson = (r: RichResultSet) => J(Map("id" -> r.nextInt.get, "randomNumber" -> r.nextInt.get))

  @inline private[this] final def asJson(id: Int, randomNumber: Int) = J(Map("id" -> id, "randomNumber" -> randomNumber))

  @inline private[this] final def next = random.nextInt(1, 10001)

  private[this] final val selectsql = "select id, randomNumber from World where id = ?"

  private[this] final val updatesql = "update World set randomNumber = ? where id = ?"

  private[this] final val datasource = "MysqlBenchmark"

}
