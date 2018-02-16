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
import java.util.List;
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
    ServerMode serverMode = ServerMode.valueOf(args[0]);
    Properties config = new Properties();
    try (InputStream in =
             Thread.currentThread()
                   .getContextClassLoader()
                   .getResourceAsStream("hello/server.properties")) {
      config.load(in);
    }
    int port = Integer.parseInt(config.getProperty("undertow.port"));
    String host = config.getProperty("undertow.host");
    HttpHandler pathHandler = serverMode.newPathHandler(config);
    HttpHandler rootHandler = new SetHeaderHandler(pathHandler, "Server", "U-tow");
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

  enum ServerMode {
    /**
     * The server will only implement the test types that do not require a
     * database.
     */
    NO_DATABASE {
      @Override
      HttpHandler newPathHandler(Properties config) {
        return new PathHandler()
            .addExactPath("/plaintext", new PlaintextHandler())
            .addExactPath("/json",      new JsonHandler());
      }
    },

    /**
     * The server will use a MySQL database and will only implement the test
     * types that require a database.
     */
    MYSQL {
      @Override
      HttpHandler newPathHandler(Properties config) {
        String jdbcUrl = config.getProperty("mysql.jdbcUrl");
        String username = config.getProperty("mysql.username");
        String password = config.getProperty("mysql.password");
        int connections = Integer.parseInt(config.getProperty("mysql.connections"));
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
    POSTGRESQL {
      @Override
      HttpHandler newPathHandler(Properties config) {
        String jdbcUrl = config.getProperty("postgresql.jdbcUrl");
        String username = config.getProperty("postgresql.username");
        String password = config.getProperty("postgresql.password");
        int connections = Integer.parseInt(config.getProperty("postgresql.connections"));
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
    MONGODB {
      @Override
      HttpHandler newPathHandler(Properties config) {
        String host = config.getProperty("mongodb.host");
        String databaseName = config.getProperty("mongodb.databaseName");
        int connections = Integer.parseInt(config.getProperty("mongodb.connections"));
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
    MONGODB_ASYNC {
      @Override
      HttpHandler newPathHandler(Properties config) {
        String host = config.getProperty("mongodb.host");
        String databaseName = config.getProperty("mongodb.databaseName");
        int connections = Integer.parseInt(config.getProperty("mongodb.connections"));
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
     * @param config the server configuration
     */
    abstract HttpHandler newPathHandler(Properties config);

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
              .hosts(List.of(new ServerAddress(host)))
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
