package hello;

import static io.undertow.util.Headers.CONTENT_TYPE;
import static java.nio.charset.StandardCharsets.US_ASCII;
import static java.util.Comparator.comparing;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.mustachejava.DefaultMustacheFactory;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import io.undertow.Undertow;
import io.undertow.UndertowOptions;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.server.handlers.BlockingHandler;
import io.undertow.server.handlers.PathHandler;
import io.undertow.server.handlers.SetHeaderHandler;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;
import java.util.StringJoiner;
import java.util.concurrent.ThreadLocalRandom;
import javax.sql.DataSource;

public final class HelloWebServer {
  private HelloWebServer() {
    throw new AssertionError();
  }

  enum Mode { NO_DATABASE, POSTGRESQL }

  public static void main(String[] args) {
    var mode = Mode.valueOf(args[0]);
    var handler = serverHeaderHandler(pathHandler(mode));

    Undertow
        .builder()
        .addHttpListener(8080, "0.0.0.0")
        .setIoThreads(Runtime.getRuntime().availableProcessors() * 2)
        // In HTTP/1.1, connections are persistent unless declared otherwise.
        // Adding a "Connection: keep-alive" header to every response would only
        // add useless bytes.
        .setServerOption(UndertowOptions.ALWAYS_SET_KEEP_ALIVE, false)
        .setHandler(handler)
        .build()
        .start();
  }

  static HttpHandler serverHeaderHandler(HttpHandler next) {
    return new SetHeaderHandler(next, "Server", "U-tow");
  }

  static HttpHandler pathHandler(Mode mode) {
    return switch (mode) {
      case NO_DATABASE -> noDatabasePathHandler();
      case POSTGRESQL -> postgresqlPathHandler();
    };
  }

  static HttpHandler noDatabasePathHandler() {
    return new PathHandler()
        .addExactPath("/plaintext", plaintextHandler())
        .addExactPath("/json", jsonHandler());
  }

  static HttpHandler postgresqlPathHandler() {
    var config = new HikariConfig();
    config.setJdbcUrl("jdbc:postgresql://tfb-database:5432/hello_world");
    config.setUsername("benchmarkdbuser");
    config.setPassword("benchmarkdbpass");
    config.setMaximumPoolSize(48);

    var db = new HikariDataSource(config);

    return new BlockingHandler(
        new PathHandler()
            .addExactPath("/db", dbHandler(db))
            .addExactPath("/queries", queriesHandler(db))
            .addExactPath("/fortunes", fortunesHandler(db))
            .addExactPath("/updates", updatesHandler(db)));
  }

  static HttpHandler plaintextHandler() {
    var text = "Hello, World!";
    var bytes = text.getBytes(US_ASCII);
    var buffer = ByteBuffer.allocateDirect(bytes.length)
                           .put(bytes)
                           .flip();

    return exchange -> {
      exchange.getResponseHeaders().put(CONTENT_TYPE, "text/plain");
      exchange.getResponseSender().send(buffer.duplicate());
    };
  }

  static HttpHandler jsonHandler() {
    return exchange -> {
      var value = Map.of("message", "Hello, World!");
      sendJson(exchange, value);
    };
  }

  static HttpHandler dbHandler(DataSource db) {
    Objects.requireNonNull(db);

    return exchange -> {
      World world;

      try (var connection = db.getConnection();
           var statement =
               connection.prepareStatement(
                   "SELECT * FROM world WHERE id = ?")) {

        statement.setInt(1, randomWorldNumber());
        try (var resultSet = statement.executeQuery()) {
          resultSet.next();
          var id = resultSet.getInt("id");
          var randomNumber = resultSet.getInt("randomnumber");
          world = new World(id, randomNumber);
        }
      }

      sendJson(exchange, world);
    };
  }

  static HttpHandler queriesHandler(DataSource db) {
    Objects.requireNonNull(db);

    return exchange -> {
      var worlds = new World[getQueries(exchange)];

      try (var connection = db.getConnection();
           var statement =
               connection.prepareStatement(
                   "SELECT * FROM world WHERE id = ?")) {

        for (var i = 0; i < worlds.length; i++) {
          statement.setInt(1, randomWorldNumber());
          try (var resultSet = statement.executeQuery()) {
            resultSet.next();
            var id = resultSet.getInt("id");
            var randomNumber = resultSet.getInt("randomnumber");
            worlds[i] = new World(id, randomNumber);
          }
        }
      }

      sendJson(exchange, worlds);
    };
  }

  static HttpHandler updatesHandler(DataSource db) {
    Objects.requireNonNull(db);

    return exchange -> {
      var worlds = new World[getQueries(exchange)];

      try (var connection = db.getConnection()) {
        try (var statement =
                 connection.prepareStatement(
                     "SELECT * FROM world WHERE id = ?")) {

          for (int i = 0; i < worlds.length; i++) {
            statement.setInt(1, randomWorldNumber());
            try (var resultSet = statement.executeQuery()) {
              resultSet.next();
              var id = resultSet.getInt("id");
              var randomNumber = resultSet.getInt("randomnumber");
              worlds[i] = new World(id, randomNumber);
            }
          }
        }

        var updateSql = new StringJoiner(
            ", ",
            "UPDATE world SET randomnumber = temp.randomnumber FROM (VALUES ",
            " ORDER BY 1) AS temp(id, randomnumber) WHERE temp.id = world.id");

        for (var world : worlds) {
          updateSql.add("(?, ?)");
        }

        try (var statement = connection.prepareStatement(updateSql.toString())) {
          var i = 0;
          for (var world : worlds) {
            world.randomNumber = randomWorldNumber();
            statement.setInt(++i, world.id);
            statement.setInt(++i, world.randomNumber);
          }
          statement.executeUpdate();
        }
      }

      sendJson(exchange, worlds);
    };
  }

  static HttpHandler fortunesHandler(DataSource db) {
    Objects.requireNonNull(db);

    var mustacheFactory = new DefaultMustacheFactory();

    return exchange -> {
      var fortunes = new ArrayList<Fortune>();

      try (var connection = db.getConnection();
           var statement = connection.prepareStatement("SELECT * FROM fortune");
           var resultSet = statement.executeQuery()) {

        while (resultSet.next()) {
          var id = resultSet.getInt("id");
          var message = resultSet.getString("message");
          fortunes.add(new Fortune(id, message));
        }
      }

      fortunes.add(new Fortune(0, "Additional fortune added at request time."));
      fortunes.sort(comparing(fortune -> fortune.message));

      var mustache = mustacheFactory.compile("hello/fortunes.mustache");
      var writer = new StringWriter();
      mustache.execute(writer, fortunes);
      var html = writer.toString();

      exchange.getResponseHeaders().put(CONTENT_TYPE, "text/html;charset=utf-8");
      exchange.getResponseSender().send(html);
    };
  }

  static int getQueries(HttpServerExchange exchange) {
    var values = exchange.getQueryParameters().get("queries");
    if (values == null)
      return 1;

    var textValue = values.peekFirst();
    if (textValue == null)
      return 1;

    int parsedValue;
    try {
      parsedValue = Integer.parseInt(textValue);
    } catch (NumberFormatException e) {
      return 1;
    }

    return Math.min(500, Math.max(1, parsedValue));
  }

  static int randomWorldNumber() {
    return 1 + ThreadLocalRandom.current().nextInt(10000);
  }

  static void sendJson(HttpServerExchange exchange, Object value)
      throws IOException {

    var bytes = objectMapper.writeValueAsBytes(value);
    var buffer = ByteBuffer.wrap(bytes);

    exchange.getResponseHeaders().put(CONTENT_TYPE, "application/json");
    exchange.getResponseSender().send(buffer);
  }

  private static final ObjectMapper objectMapper = new ObjectMapper();

  public record Fortune(int id, String message) {
    public Fortune {
      Objects.requireNonNull(message);
    }
  }

  public static final class World {
    public final int id;
    public int randomNumber; // non-final, so this class can't be a record

    public World(int id, int randomNumber) {
      this.id = id;
      this.randomNumber = randomNumber;
    }
  }
}
