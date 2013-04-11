package hello

import javax.sql.DataSource
import com.mysql.jdbc.jdbc2.optional.MysqlConnectionPoolDataSource
import javax.naming.InitialContext


sealed trait DataSourceProvider {

  def dataSource: DataSource

}

// Used for tests
trait TestDataSourceProvider extends DataSourceProvider {

  val dataSource: DataSource = {
    val ds = new MysqlConnectionPoolDataSource
    ds.setUser("root")
    ds.setServerName("localhost")
    ds.setDatabaseName("hello_world")
    ds
  }

}

/**
 * JNDI lookup
 */
trait JndiDataSourceProvider extends DataSourceProvider {

  lazy val dataSource = new InitialContext().lookup("java:comp/env/jdbc/hello_world").asInstanceOf[DataSource]

}
