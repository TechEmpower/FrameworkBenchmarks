package com.ibm.techempower

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.xml._

import com.ibm.plain.rest.{ Resource, Html }
import com.ibm.plain.jdbc.withConnection
import com.ibm.plain.jdbc.ConnectionHelper._

final class Fortunes

    extends Resource {

  Get {
    val list = new ListBuffer[(Int, String)]
    withConnection(datasource) { implicit c => for (row <- sql ! asRow) { list += row } }
    list += ((0, "Additional fortune added at request time."))
    html(rows(list.sortBy(_._2)))
  }

  @inline private[this] final def asRow = (r: RichResultSet) => (r.nextInt, r.nextString)

  @inline private[this] final def rows(list: ListBuffer[(Int, String)]) = list.map { e => row(e._1, e._2) }

  @inline private[this] final def row(id: Int, message: String) = <tr><td>{ id }</td><td>{ message }</td></tr>

  @inline private[this] final def html(rows: ListBuffer[Elem]): Html =
    <html>
      <head><title>Fortunes</title></head>
      <body>  <table>
                <tr><th>id</th><th>message</th></tr>
                { rows }
              </table> </body>
    </html>

  private[this] final val sql = "select id, message from Fortune"

  private[this] final val datasource = "MysqlBenchmark"

}
