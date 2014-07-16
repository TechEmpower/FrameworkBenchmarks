package hello;

import io.undertow.server.HttpServerExchange;
import org.apache.commons.dbcp.ConnectionFactory;
import org.apache.commons.dbcp.DriverManagerConnectionFactory;
import org.apache.commons.dbcp.PoolableConnectionFactory;
import org.apache.commons.dbcp.PoolingDataSource;
import org.apache.commons.pool.impl.GenericObjectPool;

import javax.sql.DataSource;
import java.util.Deque;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Provides utility methods for the benchmark tests.
 */
final class Helper {
  private Helper() {
    throw new AssertionError();
  }

  /**
   * Constructs a new SQL data source with the given parameters.  Connections
   * to this data source are pooled.
   *
   * @param uri the URI for database connections
   * @param user the username for the database
   * @param password the password for the database
   * @return a new SQL data source
   */
  static DataSource newDataSource(String uri,
                                  String user,
                                  String password) {
    GenericObjectPool connectionPool = new GenericObjectPool();
    connectionPool.setMaxActive(256);
    connectionPool.setMaxIdle(256);
    ConnectionFactory connectionFactory = new DriverManagerConnectionFactory(
        uri, user, password);
    //
    // This constructor modifies the connection pool, setting its connection
    // factory to this.  (So despite how it may appear, all of the objects
    // declared in this method are incorporated into the returned result.)
    //
    new PoolableConnectionFactory(
        connectionFactory, connectionPool, null, null, false, true);
    return new PoolingDataSource(connectionPool);
  }

  /**
   * Returns the value of the "queries" request parameter, which is an integer
   * bound between 1 and 500 with a default value of 1.
   *
   * @param exchange the current HTTP exchange
   * @return the value of the "queries" request parameter
   */
  static int getQueries(HttpServerExchange exchange) {
    Deque<String> values = exchange.getQueryParameters().get("queries");
    if (values == null) {
      return 1;
    }
    String textValue = values.peekFirst();
    if (textValue == null) {
      return 1;
    }
    try {
      int parsedValue = Integer.parseInt(textValue);
      return Math.min(500, Math.max(1, parsedValue));
    } catch (NumberFormatException e) {
      return 1;
    }
  }

  /**
   * Returns a random integer that is a suitable value for both the {@code id}
   * and {@code randomNumber} properties of a world object.
   *
   * @return a random world number
   */
  static int randomWorld() {
    return 1 + ThreadLocalRandom.current().nextInt(10000);
  }
}
