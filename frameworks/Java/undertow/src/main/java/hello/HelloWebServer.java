package hello;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.MustacheFactory;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.net.MediaType;
import com.mongodb.DB;
import com.mongodb.MongoClient;
import io.undertow.Handlers;
import io.undertow.Undertow;
import io.undertow.UndertowOptions;
import io.undertow.util.Headers;
import org.xnio.Options;

import javax.sql.DataSource;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Properties;

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

  //MediaType.toString() does non-trivial work and does not cache the result
  //so we cache it here
  public static final String JSON_UTF8 = MediaType.JSON_UTF_8.toString();

  public static final String TEXT_PLAIN = MediaType.PLAIN_TEXT_UTF_8.toString();

  public static final String HTML_UTF8 = MediaType.HTML_UTF_8.toString();

  public static void main(String[] args) throws Exception {
    new HelloWebServer();
  }

  /**
   * Creates and starts a new web server whose configuration is specified in the
   * {@code server.properties} file.
   *
   * @throws IOException if the application properties file cannot be read or
   *                     the Mongo database hostname cannot be resolved
   * @throws SQLException if reading from the SQL database (while priming the
   *                      cache) fails
   */
  public HelloWebServer() throws ClassNotFoundException, IOException, SQLException {
    Class.forName("org.postgresql.Driver");
    Properties properties = new Properties();
    try (InputStream in = HelloWebServer.class.getResourceAsStream(
        "server.properties")) {
      properties.load(in);
    }
    final ObjectMapper objectMapper = new ObjectMapper();
    final MustacheFactory mustacheFactory = new DefaultMustacheFactory();
    final DataSource mysql = Helper.newDataSource(
        properties.getProperty("mysql.uri"),
        properties.getProperty("mysql.user"),
        properties.getProperty("mysql.password"));
    final DataSource postgresql = Helper.newDataSource(
        properties.getProperty("postgresql.uri"),
        properties.getProperty("postgresql.user"),
        properties.getProperty("postgresql.password"));
    final DB mongodb = new MongoClient(properties.getProperty("mongodb.host"))
        .getDB(properties.getProperty("mongodb.name"));
    //
    // The world cache is primed at startup with all values.  It doesn't
    // matter which database backs it; they all contain the same information
    // and the CacheLoader.load implementation below is never invoked.
    //
    final LoadingCache<Integer, World> worldCache = CacheBuilder.newBuilder()
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
        .addHttpListener(
            Integer.parseInt(properties.getProperty("web.port")),
            properties.getProperty("web.host"))
        .setBufferSize(1024 * 16)
        .setIoThreads(Runtime.getRuntime().availableProcessors() * 2) //this seems slightly faster in some configurations
        .setSocketOption(Options.BACKLOG, 10000)
        .setServerOption(UndertowOptions.ALWAYS_SET_KEEP_ALIVE, false) //don't send a keep-alive header for HTTP/1.1 requests, as it is not required
        .setServerOption(UndertowOptions.ALWAYS_SET_DATE, true)
        .setServerOption(UndertowOptions.ENABLE_CONNECTOR_STATISTICS, false)
        .setServerOption(UndertowOptions.RECORD_REQUEST_START_TIME, false)
        .setHandler(Handlers.header(Handlers.path()
            .addPrefixPath("/json",
                new JsonHandler(objectMapper))
            .addPrefixPath("/db/mysql",
                new DbSqlHandler(objectMapper, mysql, false))
            .addPrefixPath("/queries/mysql",
                new DbSqlHandler(objectMapper, mysql, true))
            .addPrefixPath("/db/postgresql",
                new DbSqlHandler(objectMapper, postgresql, false))
            .addPrefixPath("/queries/postgresql",
                new DbSqlHandler(objectMapper, postgresql, true))
            .addPrefixPath("/db/mongodb",
                new DbMongoHandler(objectMapper, mongodb, false))
            .addPrefixPath("/queries/mongodb",
                new DbMongoHandler(objectMapper, mongodb, true))
            .addPrefixPath("/fortunes/mysql",
                new FortunesSqlHandler(mustacheFactory, mysql))
            .addPrefixPath("/fortunes/postgresql",
                new FortunesSqlHandler(mustacheFactory, postgresql))
            .addPrefixPath("/fortunes/mongodb",
                new FortunesMongoHandler(mustacheFactory, mongodb))
            .addPrefixPath("/updates/mysql",
                new UpdatesSqlHandler(objectMapper, mysql))
            .addPrefixPath("/updates/postgresql",
                new UpdatesSqlHandler(objectMapper, postgresql))
            .addPrefixPath("/updates/mongodb",
                new UpdatesMongoHandler(objectMapper, mongodb))
            .addPrefixPath("/plaintext",
                new PlaintextHandler())
            .addPrefixPath("/cache",
                new CacheHandler(objectMapper, worldCache)),
            Headers.SERVER_STRING, "U-tow"))
        .setWorkerThreads(200)
        .build()
        .start();
  }
}
