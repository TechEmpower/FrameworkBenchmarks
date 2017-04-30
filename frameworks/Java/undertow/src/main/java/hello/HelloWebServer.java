package hello;

import com.mongodb.MongoClient;
import com.mongodb.MongoClientOptions;
import com.mongodb.ServerAddress;
import com.mongodb.async.client.MongoClientSettings;
import com.mongodb.async.client.MongoClients;
import com.mongodb.client.MongoDatabase;
import com.mongodb.connection.ClusterConnectionMode;
import com.mongodb.connection.ClusterSettings;
import com.mongodb.connection.ConnectionPoolSettings;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import io.undertow.Undertow;
import io.undertow.UndertowOptions;
import io.undertow.server.HttpHandler;
import io.undertow.server.handlers.BlockingHandler;
import io.undertow.server.handlers.PathHandler;
import io.undertow.server.handlers.SetHeaderHandler;
import java.io.InputStream;
import java.util.Collections;
import java.util.Properties;
import javax.sql.DataSource;

/**
 * Provides the {@link #main(String[])} method, which launches the application.
 */
public final class HelloWebServer {
  private HelloWebServer() {
    throw new AssertionError();
  }

  public static void main(String[] args) throws Exception {
    Mode mode = Mode.valueOf(args[0]);
    Properties props = new Properties();
    try (InputStream in =
             Thread.currentThread()
                   .getContextClassLoader()
                   .getResourceAsStream("hello/server.properties")) {
      props.load(in);
    }
    int port = Integer.parseInt(props.getProperty("undertow.port"));
    String host = props.getProperty("undertow.host");
    HttpHandler paths = mode.paths(props);
    HttpHandler rootHandler = new SetHeaderHandler(paths, "Server", "U-tow");
    Undertow.builder()
            .addHttpListener(port, host)
            // In HTTP/1.1, connections are persistent unless declared
            // otherwise.  Adding a "Connection: keep-alive" header to every
            // response would only add useless bytes.
            .setServerOption(UndertowOptions.ALWAYS_SET_KEEP_ALIVE, false)
            .setHandler(rootHandler)
            .build()
            .start();
  }

  enum Mode {
    /**
     * The server will only implement the test types that do not require a
     * database.
     */
    NO_DATABASE() {
      @Override
      HttpHandler paths(Properties props) {
        return new PathHandler()
            .addExactPath("/plaintext", new PlaintextHandler())
            .addExactPath("/json",      new JsonHandler());
      }
    },

    /**
     * The server will use a MySQL database and will only implement the test
     * types that require a database.
     */
    MYSQL() {
      @Override
      HttpHandler paths(Properties props) {
        String jdbcUrl = props.getProperty("mysql.jdbcUrl");
        String username = props.getProperty("mysql.username");
        String password = props.getProperty("mysql.password");
        int connections = Integer.parseInt(props.getProperty("mysql.connections"));
        DataSource db = newSqlDataSource(jdbcUrl, username, password, connections);
        return new PathHandler()
            .addExactPath("/db",       new BlockingHandler(new DbSqlHandler(db)))
            .addExactPath("/queries",  new BlockingHandler(new QueriesSqlHandler(db)))
            .addExactPath("/fortunes", new BlockingHandler(new FortunesSqlHandler(db)))
            .addExactPath("/updates",  new BlockingHandler(new UpdatesSqlHandler(db)));
      }
    },

    /**
     * The server will use a PostgreSQL database and will only implement the
     * test types that require a database.
     */
    POSTGRESQL() {
      @Override
      HttpHandler paths(Properties props) {
        String jdbcUrl = props.getProperty("postgresql.jdbcUrl");
        String username = props.getProperty("postgresql.username");
        String password = props.getProperty("postgresql.password");
        int connections = Integer.parseInt(props.getProperty("postgresql.connections"));
        DataSource db = newSqlDataSource(jdbcUrl, username, password, connections);
        return new PathHandler()
            .addExactPath("/db",       new BlockingHandler(new DbSqlHandler(db)))
            .addExactPath("/queries",  new BlockingHandler(new QueriesSqlHandler(db)))
            .addExactPath("/fortunes", new BlockingHandler(new FortunesSqlHandler(db)))
            .addExactPath("/updates",  new BlockingHandler(new UpdatesSqlHandler(db)));
      }
    },

    /**
     * The server will use a MongoDB database and will only implement the test
     * types that require a database.
     */
    MONGODB() {
      @Override
      HttpHandler paths(Properties props) {
        String host = props.getProperty("mongodb.host");
        String databaseName = props.getProperty("mongodb.databaseName");
        int connections = Integer.parseInt(props.getProperty("mongodb.connections"));
        MongoDatabase db = newMongoDatabase(host, databaseName, connections);
        return new PathHandler()
            .addExactPath("/db",       new BlockingHandler(new DbMongoHandler(db)))
            .addExactPath("/queries",  new BlockingHandler(new QueriesMongoHandler(db)))
            .addExactPath("/fortunes", new BlockingHandler(new FortunesMongoHandler(db)))
            .addExactPath("/updates",  new BlockingHandler(new UpdatesMongoHandler(db)));
      }
    },

    /**
     * The server will use a MongoDB database with an asynchronous API and will
     * only implement the test types that require a database.
     */
    MONGODB_ASYNC() {
      @Override
      HttpHandler paths(Properties props) {
        String host = props.getProperty("mongodb.host");
        String databaseName = props.getProperty("mongodb.databaseName");
        int connections = Integer.parseInt(props.getProperty("mongodb.connections"));
        com.mongodb.async.client.MongoDatabase db =
            newMongoDatabaseAsync(host, databaseName, connections);
        return new PathHandler()
            .addExactPath("/db",       new AsyncHandler(new DbMongoAsyncHandler(db)))
            .addExactPath("/queries",  new AsyncHandler(new QueriesMongoAsyncHandler(db)))
            .addExactPath("/fortunes", new AsyncHandler(new FortunesMongoAsyncHandler(db)))
            .addExactPath("/updates",  new AsyncHandler(new UpdatesMongoAsyncHandler(db)));
      }
    };

    /**
     * Returns an HTTP handler that provides routing for all the
     * test-type-specific endpoints of the server.
     *
     * @param props the server configuration
     */
    abstract HttpHandler paths(Properties props);

    /**
     * Provides a source of connections to a SQL database.
     */
    static DataSource newSqlDataSource(String jdbcUrl,
                                       String username,
                                       String password,
                                       int connections) {
      HikariConfig config = new HikariConfig();
      config.setJdbcUrl(jdbcUrl);
      config.setUsername(username);
      config.setPassword(password);
      config.setMaximumPoolSize(connections);
      return new HikariDataSource(config);
    }

    /**
     * Provides a source of connections to a MongoDB database.
     */
    static MongoDatabase newMongoDatabase(String host,
                                          String databaseName,
                                          int connections) {
      MongoClientOptions.Builder options = MongoClientOptions.builder();
      options.connectionsPerHost(connections);
      options.threadsAllowedToBlockForConnectionMultiplier(
          (int) Math.ceil((double) MAX_DB_REQUEST_CONCURRENCY / connections));
      MongoClient client = new MongoClient(host, options.build());
      return client.getDatabase(databaseName);
    }

    /**
     * Provides a source of connections to a MongoDB database with an
     * asynchronous API.
     */
    static com.mongodb.async.client.MongoDatabase
    newMongoDatabaseAsync(String host,
                          String databaseName,
                          int connections) {
      ClusterSettings clusterSettings =
          ClusterSettings
              .builder()
              .mode(ClusterConnectionMode.SINGLE)
              .hosts(Collections.singletonList(new ServerAddress(host)))
              .build();
      ConnectionPoolSettings connectionPoolSettings =
          ConnectionPoolSettings
              .builder()
              .maxSize(connections)
              .maxWaitQueueSize(
                  MAX_DB_REQUEST_CONCURRENCY * MAX_DB_QUERIES_PER_REQUEST)
              .build();
      MongoClientSettings clientSettings =
          MongoClientSettings
              .builder()
              .clusterSettings(clusterSettings)
              .connectionPoolSettings(connectionPoolSettings)
              .build();
      com.mongodb.async.client.MongoClient client =
          MongoClients.create(clientSettings);
      return client.getDatabase(databaseName);
    }

    private static final int MAX_DB_REQUEST_CONCURRENCY = 256;
    private static final int MAX_DB_QUERIES_PER_REQUEST = 20;
  }
}
