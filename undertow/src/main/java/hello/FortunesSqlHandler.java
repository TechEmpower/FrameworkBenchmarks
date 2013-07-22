package hello;

import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;
import com.google.common.net.MediaType;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

import javax.sql.DataSource;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import static hello.HelloWebServer.HTML_UTF8;

/**
 * Handles the fortunes test using a SQL database.
 */
final class FortunesSqlHandler implements HttpHandler {
  private final MustacheFactory mustacheFactory;
  private final DataSource database;

  FortunesSqlHandler(MustacheFactory mustacheFactory, DataSource database) {
    this.mustacheFactory = Objects.requireNonNull(mustacheFactory);
    this.database = Objects.requireNonNull(database);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) throws Exception {
    if (exchange.isInIoThread()) {
      exchange.dispatch(this);
      return;
    }
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
        Headers.CONTENT_TYPE, HTML_UTF8);
    exchange.getResponseSender().send(writer.toString());
  }
}
