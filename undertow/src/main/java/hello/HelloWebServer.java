package hello;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.net.MediaType;
import com.mongodb.BasicDBObject;
import com.mongodb.DB;
import com.mongodb.DBCursor;
import com.mongodb.DBObject;
import com.mongodb.MongoClient;
import io.undertow.Handlers;
import io.undertow.Undertow;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;
import org.apache.commons.dbcp.ConnectionFactory;
import org.apache.commons.dbcp.DriverManagerConnectionFactory;
import org.apache.commons.dbcp.PoolableConnectionFactory;
import org.apache.commons.dbcp.PoolingDataSource;
import org.apache.commons.pool.impl.GenericObjectPool;

import javax.sql.DataSource;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ThreadLocalRandom;

/**
 * An implementation of the TechEmpower benchmark tests using the Undertow web
 * server.  The only test that truly exercises Undertow in isolation is the
 * plaintext test.  For the rest, it uses best-of-breed components that are
 * expected to perform well.  The idea is that using these components enables
 * these tests to serve as performance baselines for hypothetical, Undertow-based
 * frameworks.  For instance, it is unlikely that such frameworks would complete
 * the JSON test faster than this will, because this implementation uses
 * Undertow and Jackson in the most direct way possible to fulfill the test
 * requirements.
 */
public final class HelloWebServer {

  public static void main(String[] args) throws Exception {
    new HelloWebServer();
  }

  // Helper methods

  /**
   * Constructs a new SQL data source with the given parameters.  Connections
   * to this data source are pooled.
   *
   * @param uri the URI for database connections
   * @param user the username for the database
   * @param password the password for the database
   * @return a new SQL data source
   */
  private static DataSource newDataSource(String uri,
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
  private static int getQueries(HttpServerExchange exchange) {
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
  private static int randomWorld() {
    return 1 + ThreadLocalRandom.current().nextInt(10000);
  }

  // Fields and constructor

  private final ObjectMapper objectMapper = new ObjectMapper();
  private final MustacheFactory mustacheFactory = new DefaultMustacheFactory();
  private final DataSource mysql;
  private final DataSource postgresql;
  private final DB mongodb;
  private final LoadingCache<Integer, World> worldCache;

  /**
   * Creates and starts a new web server whose configuration is specified in the
   * {@code server.properties} file.
   *
   * @throws IOException if the application properties file cannot be read or
   *                     the Mongo database hostname cannot be resolved
   * @throws SQLException if reading from the SQL database (while priming the
   *                      cache) fails
   */
  public HelloWebServer() throws IOException, SQLException {
    Properties properties = new Properties();
    try (InputStream in = getClass().getResourceAsStream("server.properties")) {
      properties.load(in);
    }
    mysql = newDataSource(
        properties.getProperty("mysql.uri"),
        properties.getProperty("mysql.user"),
        properties.getProperty("mysql.password"));
    postgresql = newDataSource(
        properties.getProperty("postgresql.uri"),
        properties.getProperty("postgresql.user"),
        properties.getProperty("postgresql.password"));
    mongodb = new MongoClient(properties.getProperty("mongodb.host"))
        .getDB(properties.getProperty("mongodb.name"));
    //
    // The world cache is primed at startup with all values.  It doesn't
    // matter which database backs it; they all contain the same information
    // and the CacheLoader.load implementation below is never invoked.
    //
    worldCache = CacheBuilder.newBuilder()
        .build(new CacheLoader<Integer, World>() {
          @Override
          public World load(Integer id) throws Exception {
            try (Connection connection = mysql.getConnection();
                 PreparedStatement statement = connection.prepareStatement(
                     "SELECT * FROM World WHERE id = ?",
                     ResultSet.TYPE_FORWARD_ONLY,
                     ResultSet.CONCUR_READ_ONLY)) {
              statement.setInt(1, id);
              try (ResultSet resultSet = statement.executeQuery()) {
                resultSet.next();
                return new World(
                    resultSet.getInt("id"),
                    resultSet.getInt("randomNumber"));
              }
            }
          }
        });
    try (Connection connection = mysql.getConnection();
         PreparedStatement statement = connection.prepareStatement(
             "SELECT * FROM World",
             ResultSet.TYPE_FORWARD_ONLY,
             ResultSet.CONCUR_READ_ONLY);
         ResultSet resultSet = statement.executeQuery()) {
      while (resultSet.next()) {
        World world = new World(
            resultSet.getInt("id"),
            resultSet.getInt("randomNumber"));
        worldCache.put(world.id, world);
      }
    }
    Undertow.builder()
        .addListener(
            Integer.parseInt(properties.getProperty("web.port")),
            properties.getProperty("web.host"))
        .setBufferSize(1024 * 16)
        //
        // TODO: Figure out the best values for IoThreads and WorkerThreads.
        //
        // It is probably some function of the number of processor cores and the
        // expected client-side concurrency.  The values below seemed to do the
        // best out of several that we tried in spot tests on our 8-core i7
        // hardware.
        //
        .setIoThreads(64)
        .setWorkerThreads(256)
        .setHandler(Handlers.date(Handlers.header(Handlers.path()
            .addPath("/json", new HttpHandler() {
              @Override
              public void handleRequest(HttpServerExchange exchange)
                  throws Exception {
                handleJsonTest(exchange);
              }
            })
            .addPath("/db/mysql", new HttpHandler() {
              @Override
              public void handleRequest(HttpServerExchange exchange)
                  throws Exception {
                handleDbTest(exchange, mysql);
              }
            })
            .addPath("/db/postgresql", new HttpHandler() {
              @Override
              public void handleRequest(HttpServerExchange exchange)
                  throws Exception {
                handleDbTest(exchange, postgresql);
              }
            })
            .addPath("/db/mongodb", new HttpHandler() {
              @Override
              public void handleRequest(HttpServerExchange exchange)
                  throws Exception {
                handleDbTest(exchange, mongodb);
              }
            })
            .addPath("/fortunes/mysql", new HttpHandler() {
              @Override
              public void handleRequest(HttpServerExchange exchange)
                  throws Exception {
                handleFortunesTest(exchange, mysql);
              }
            })
            .addPath("/fortunes/postgresql", new HttpHandler() {
              @Override
              public void handleRequest(HttpServerExchange exchange)
                  throws Exception {
                handleFortunesTest(exchange, postgresql);
              }
            })
            .addPath("/fortunes/mongodb", new HttpHandler() {
              @Override
              public void handleRequest(HttpServerExchange exchange)
                  throws Exception {
                handleFortunesTest(exchange, mongodb);
              }
            })
            .addPath("/updates/mysql", new HttpHandler() {
              @Override
              public void handleRequest(HttpServerExchange exchange)
                  throws Exception {
                handleUpdatesTest(exchange, mysql);
              }
            })
            .addPath("/updates/postgresql", new HttpHandler() {
              @Override
              public void handleRequest(HttpServerExchange exchange)
                  throws Exception {
                handleUpdatesTest(exchange, postgresql);
              }
            })
            .addPath("/updates/mongodb", new HttpHandler() {
              @Override
              public void handleRequest(HttpServerExchange exchange)
                  throws Exception {
                handleUpdatesTest(exchange, mongodb);
              }
            })
            .addPath("/plaintext", new HttpHandler() {
              @Override
              public void handleRequest(HttpServerExchange exchange)
                  throws Exception {
                handlePlaintextTest(exchange);
              }
            })
            .addPath("/cache", new HttpHandler() {
              @Override
              public void handleRequest(HttpServerExchange exchange)
                  throws Exception {
                handleCacheTest(exchange);
              }
            }), Headers.SERVER_STRING, "undertow")))
        .build()
        .start();
  }

  // Request handlers

  /**
   * Completes the JSON test for the given request.
   *
   * @param exchange the current HTTP exchange
   * @throws IOException if JSON encoding fails
   */
  private void handleJsonTest(HttpServerExchange exchange) throws IOException {
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, MediaType.JSON_UTF_8.toString());
    exchange.getResponseSender().send(
        objectMapper.writeValueAsString(
            Collections.singletonMap("message", "Hello, World!")));
  }

  /**
   * Completes the database query test for the given request using a SQL
   * database.  This handler is used for both the single-query and
   * multiple-query tests.
   *
   * @param exchange the current HTTP exchange
   * @param database the SQL database
   * @throws IOException if JSON encoding fails
   * @throws SQLException if reading from the database fails
   */
  private void handleDbTest(HttpServerExchange exchange, DataSource database)
      throws IOException, SQLException {
    int queries = getQueries(exchange);
    World[] worlds = new World[queries];
    try (Connection connection = database.getConnection();
         PreparedStatement statement = connection.prepareStatement(
             "SELECT * FROM World WHERE id = ?",
             ResultSet.TYPE_FORWARD_ONLY,
             ResultSet.CONCUR_READ_ONLY)) {
      for (int i = 0; i < queries; i++) {
        statement.setInt(1, randomWorld());
        try (ResultSet resultSet = statement.executeQuery()) {
          resultSet.next();
          worlds[i] = new World(
              resultSet.getInt("id"),
              resultSet.getInt("randomNumber"));
        }
      }
    }
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, MediaType.JSON_UTF_8.toString());
    exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
  }

  /**
   * Completes the database query test for the given request using a Mongo
   * database.  This handler is used for both the single-query and
   * multiple-query tests.
   *
   * @param exchange the current HTTP exchange
   * @param database the Mongo database
   * @throws IOException if JSON encoding fails
   */
  private void handleDbTest(HttpServerExchange exchange, DB database)
      throws IOException {
    int queries = getQueries(exchange);
    World[] worlds = new World[queries];
    for (int i = 0; i < queries; i++) {
      DBObject object = database.getCollection("world").findOne(
          new BasicDBObject("id", randomWorld()));
      worlds[i] = new World(
          //
          // The creation script for the Mongo database inserts these numbers as
          // JavaScript numbers, which resolve to doubles in Java.
          //
          ((Number) object.get("id")).intValue(),
          ((Number) object.get("randomNumber")).intValue());
    }
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, MediaType.JSON_UTF_8.toString());
    exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
  }

  /**
   * Completes the fortunes test for the given request using a SQL database.
   *
   * @param exchange the current HTTP exchange
   * @param database the SQL database
   * @throws SQLException if reading from the database fails
   */
  private void handleFortunesTest(HttpServerExchange exchange,
                                  DataSource database)
      throws SQLException {
    List<Fortune> fortunes = new ArrayList<>();
    try (Connection connection = database.getConnection();
         PreparedStatement statement = connection.prepareStatement(
             "SELECT * FROM Fortune",
             ResultSet.TYPE_FORWARD_ONLY,
             ResultSet.CONCUR_READ_ONLY);
         ResultSet resultSet = statement.executeQuery()) {
      while (resultSet.next()) {
        fortunes.add(new Fortune(
            resultSet.getInt("id"),
            resultSet.getString("message")));
      }
    }
    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    Collections.sort(fortunes);
    Mustache mustache = mustacheFactory.compile("hello/fortunes.mustache");
    StringWriter writer = new StringWriter();
    mustache.execute(writer, fortunes);
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, MediaType.HTML_UTF_8.toString());
    exchange.getResponseSender().send(writer.toString());
  }

  /**
   * Completes the fortunes test for the given request using a Mongo database.
   *
   * @param exchange the current HTTP exchange
   * @param database the Mongo database
   */
  private void handleFortunesTest(HttpServerExchange exchange, DB database) {
    List<Fortune> fortunes = new ArrayList<>();
    DBCursor cursor = database.getCollection("fortune").find();
    while (cursor.hasNext()) {
      DBObject object = cursor.next();
      fortunes.add(new Fortune(
          ((Number) object.get("id")).intValue(),
          (String) object.get("message")));
    }
    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    Collections.sort(fortunes);
    Mustache mustache = mustacheFactory.compile("hello/fortunes.mustache");
    StringWriter writer = new StringWriter();
    mustache.execute(writer, fortunes);
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, MediaType.HTML_UTF_8.toString());
    exchange.getResponseSender().send(writer.toString());
  }

  /**
   * Completes the database update test for the given request using a SQL
   * database.
   *
   * @param exchange the current HTTP exchange
   * @param database the SQL database
   * @throws IOException if JSON encoding fails
   * @throws SQLException if reading from or writing to the database fails
   */
  private void handleUpdatesTest(HttpServerExchange exchange,
                                 DataSource database)
      throws IOException, SQLException {
    int queries = getQueries(exchange);
    World[] worlds = new World[queries];
    try (Connection connection = database.getConnection();
         PreparedStatement query = connection.prepareStatement(
             "SELECT * FROM World WHERE id = ?",
             ResultSet.TYPE_FORWARD_ONLY,
             ResultSet.CONCUR_READ_ONLY);
         PreparedStatement update = connection.prepareStatement(
             "UPDATE World SET randomNumber = ? WHERE id= ?")) {
      for (int i = 0; i < queries; i++) {
        query.setInt(1, randomWorld());
        World world;
        try (ResultSet resultSet = query.executeQuery()) {
          resultSet.next();
          world = new World(
              resultSet.getInt("id"),
              resultSet.getInt("randomNumber"));
        }
        world.randomNumber = randomWorld();
        update.setInt(1, world.randomNumber);
        update.setInt(2, world.id);
        update.executeUpdate();
        worlds[i] = world;
      }
    }
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, MediaType.JSON_UTF_8.toString());
    exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
  }

  /**
   * Completes the database update test for the given request using a Mongo
   * database.
   *
   * @param exchange the current HTTP exchange
   * @param database the Mongo database
   * @throws IOException if JSON encoding fails
   */
  private void handleUpdatesTest(HttpServerExchange exchange, DB database)
      throws IOException {
    int queries = getQueries(exchange);
    World[] worlds = new World[queries];
    for (int i = 0; i < queries; i++) {
      int id = randomWorld();
      DBObject key = new BasicDBObject("id", id);
      //
      // The requirements for the test dictate that we must fetch the World
      // object from the data store and read its randomNumber field, even though
      // we could technically avoid doing either of those things and still
      // produce the correct output and side effects.
      //
      DBObject object = database.getCollection("world").findOne(key);
      int oldRandomNumber = ((Number) object.get("randomNumber")).intValue();
      int newRandomNumber = randomWorld();
      object.put("randomNumber", newRandomNumber);
      database.getCollection("world").update(key, object);
      worlds[i] = new World(id, newRandomNumber);
    }
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, MediaType.JSON_UTF_8.toString());
    exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
  }

  /**
   * Completes the plaintext test forthe given request.
   *
   * @param exchange the current HTTP exchange
   */
  private void handlePlaintextTest(HttpServerExchange exchange) {
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, MediaType.PLAIN_TEXT_UTF_8.toString());
    exchange.getResponseSender().send("Hello, World!");
  }

  /**
   * Completes the cache test for the given request.
   *
   * @param exchange the current HTTP exchange
   * @throws ExecutionException the reading from the cache fails
   * @throws IOException if JSON encoding fails
   */

  private void handleCacheTest(HttpServerExchange exchange)
      throws ExecutionException, IOException {
    int queries = getQueries(exchange);
    World[] worlds = new World[queries];
    for (int i = 0; i < queries; i++) {
      worlds[i] = worldCache.get(randomWorld());
    }
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, MediaType.JSON_UTF_8.toString());
    exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
  }
}
