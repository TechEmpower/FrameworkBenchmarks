package hello.model

import java.sql.{PreparedStatement, ResultSet, Connection}

case class Param[T](idx: Int, value: T)

abstract class JdbcQuery[T](mapper: (ResultSet) => T) {

  def statement: PreparedStatement

  def open(conn: Connection): JdbcQuery[T]

  def close(): JdbcQuery[T]

  def execute(param: Int) = {
    statement.setInt(1, param)
    val rs = statement.executeQuery()
    if (rs.next()) Some(mapper(rs)) else None
  }
}

sealed class ClosedQuery[T](query: String, m: (ResultSet) => T) extends JdbcQuery[T](m) {

  def statement = throw new RuntimeException("Cannot execute a closed query")

  def open(conn: Connection) = new OpenQuery[T](conn, query, m)

  def close() = this
}

sealed class OpenQuery[T](c: Connection, query: String, m: (ResultSet) => T) extends JdbcQuery[T](m) {

  val statement = c.prepareStatement(query, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)

  def open(conn: Connection) = this

  def close() = {
    c.close()
    new ClosedQuery[T](query, m)
  }
}


object SingleResultQuery {

  def byID[T](query: String)(mapper: (ResultSet) => T): JdbcQuery[T] = new ClosedQuery[T](query, mapper)

}