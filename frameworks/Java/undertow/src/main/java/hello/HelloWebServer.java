package hello;

import com.mongodb.MongoClient;
import com.mongodb.MongoClientOptions;
import com.mongodb.ServerAddress;
import com.mongodb.async.client.MongoClientSettings;
import com.mongodb.async.client.MongoClients;
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
import java.util.List;
import java.util.function.Supplier;

/**
 * Provides the {@link #main(String[])} method, which launches the application.
 */
public final class HelloWebServer {
  private HelloWebServer() {
    throw new AssertionError();
  }

  public static void main(String[] args) {
    Undertow
        .builder()
        .addHttpListener(8080, "0.0.0.0")
        // In HTTP/1.1, connections are persistent unless declared otherwise.
        // Adding a "Connection: keep-alive" header to every response would only
        // add useless bytes.
        .setServerOption(UndertowOptions.ALWAYS_SET_KEEP_ALIVE, false)
        .setHandler(ServerMode.valueOf(args[0]).createRootHandler())
        .build()
        .start();
  }

  static final int MAX_DB_REQUEST_CONCURRENCY = 512;
  static final int MAX_DB_QUERIES_PER_REQUEST = 20;

  enum ServerMode {
    /**
     * The server will only implement the test types that do not require a
     * database.
     */
    NO_DATABASE(() -> {
      var handler = new PathHandler();
      handler.addExactPath("/plaintext", new PlaintextHandler());
      handler.addExactPath("/json", new JsonHandler());
      return handler;
    }),

    /**
     * The server will use a MySQL database and will only implement the test
     * types that require a database.
     */
    MYSQL(() -> {
      var config = new HikariConfig();
      config.setJdbcUrl("jdbc:mysql://tfb-database:3306/hello_world?useSSL=false&useServerPrepStmts=true&cachePrepStmts=true");
      config.setUsername("benchmarkdbuser");
      config.setPassword("benchmarkdbpass");
      config.setMaximumPoolSize(48);

      var db = new HikariDataSource(config);

      var handler = new PathHandler();
      handler.addExactPath("/db", new BlockingHandler(new DbSqlHandler(db)));
      handler.addExactPath("/queries", new BlockingHandler(new QueriesSqlHandler(db)));
      handler.addExactPath("/fortunes", new BlockingHandler(new FortunesSqlHandler(db)));
      handler.addExactPath("/updates", new BlockingHandler(new UpdatesSqlHandler(db)));
      return handler;
    }),

    /**
     * The server will use a PostgreSQL database and will only implement the
     * test types that require a database.
     */
    POSTGRESQL(() -> {
      var config = new HikariConfig();
      config.setJdbcUrl("jdbc:postgresql://tfb-database:5432/hello_world");
      config.setUsername("benchmarkdbuser");
      config.setPassword("benchmarkdbpass");
      config.setMaximumPoolSize(48);

      var db = new HikariDataSource(config);

      var handler = new PathHandler();
      handler.addExactPath("/db", new BlockingHandler(new DbSqlHandler(db)));
      handler.addExactPath("/queries", new BlockingHandler(new QueriesSqlHandler(db)));
      handler.addExactPath("/fortunes", new BlockingHandler(new FortunesSqlHandler(db)));
      handler.addExactPath("/updates", new BlockingHandler(new UpdatesSqlHandler(db)));
      return handler;
    }),

    /**
     * The server will use a MongoDB database and will only implement the test
     * types that require a database.
     */
    MONGODB(() -> {
      int connectionPoolSize = 256;

      var options = MongoClientOptions.builder();
      options.connectionsPerHost(connectionPoolSize);
      options.threadsAllowedToBlockForConnectionMultiplier(
          (int) Math.ceil((double) MAX_DB_REQUEST_CONCURRENCY / connectionPoolSize));

      var client = new MongoClient("tfb-database:27017", options.build());

      var db = client.getDatabase("hello_world");

      var handler = new PathHandler();
      handler.addExactPath("/db", new BlockingHandler(new DbMongoHandler(db)));
      handler.addExactPath("/queries", new BlockingHandler(new QueriesMongoHandler(db)));
      handler.addExactPath("/fortunes", new BlockingHandler(new FortunesMongoHandler(db)));
      handler.addExactPath("/updates", new BlockingHandler(new UpdatesMongoHandler(db)));
      return handler;
    }),

    /**
     * The server will use a MongoDB database with an asynchronous API and will
     * only implement the test types that require a database.
     */
    MONGODB_ASYNC(() -> {
      int connectionPoolSize = 256;

      var clusterSettings =
          ClusterSettings
              .builder()
              .mode(ClusterConnectionMode.SINGLE)
              .hosts(List.of(new ServerAddress("tfb-database:27017")))
              .build();

      var connectionPoolSettings =
          ConnectionPoolSettings
              .builder()
              .maxSize(connectionPoolSize)
              .maxWaitQueueSize(MAX_DB_REQUEST_CONCURRENCY * MAX_DB_QUERIES_PER_REQUEST)
              .build();

      var clientSettings =
          MongoClientSettings
              .builder()
              .clusterSettings(clusterSettings)
              .connectionPoolSettings(connectionPoolSettings)
              .build();

      var client = MongoClients.create(clientSettings);

      var db = client.getDatabase("hello_world");

      var handler = new PathHandler();
      handler.addExactPath("/db", new AsyncHandler(new DbMongoAsyncHandler(db)));
      handler.addExactPath("/queries", new AsyncHandler(new QueriesMongoAsyncHandler(db)));
      handler.addExactPath("/fortunes", new AsyncHandler(new FortunesMongoAsyncHandler(db)));
      handler.addExactPath("/updates", new AsyncHandler(new UpdatesMongoAsyncHandler(db)));
      return handler;
    });

    private final Supplier<HttpHandler> routerSupplier;

    ServerMode(Supplier<HttpHandler> routerSupplier) {
      this.routerSupplier = routerSupplier;
    }

    /**
     * Returns an HTTP handler that provides routing for all the
     * test-type-specific endpoints of the server.
     */
    HttpHandler createRootHandler() {
      return new SetHeaderHandler(
          /* next= */ routerSupplier.get(),
          /* header= */ "Server",
          /* value= */ "U-tow");
    }
  }
}
