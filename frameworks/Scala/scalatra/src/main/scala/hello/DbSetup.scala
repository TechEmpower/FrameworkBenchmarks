package hello

import javax.sql.DataSource
import hello.model.JdbcQuery
import java.sql.Connection

trait DbSetup  {

  def dataSource: DataSource

  def useQuery[A, B](query: JdbcQuery[A])(f: (JdbcQuery[A]) => B): B = {
    val connection: Connection = if (dataSource != null) dataSource.getConnection else throw new IllegalStateException("DataSource not found")
    val opened = query.open(connection)
    val res = f(opened)
    opened.close()
    res
  }

}
