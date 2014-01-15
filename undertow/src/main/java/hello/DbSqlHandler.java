package hello;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.Objects;

import static hello.HelloWebServer.JSON_UTF8;

/**
 * Handles the single- and multiple-query database tests using a SQL database.
 */
final class DbSqlHandler implements HttpHandler {
  private final ObjectMapper objectMapper;
  private final DataSource database;

  DbSqlHandler(ObjectMapper objectMapper, DataSource database) {
    this.objectMapper = Objects.requireNonNull(objectMapper);
    this.database = Objects.requireNonNull(database);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) throws Exception {
    if (exchange.isInIoThread()) {
      exchange.dispatch(this);
      return;
    }
    int queries = Helper.getQueries(exchange);
    World[] worlds = new World[queries];
    try (Connection connection = database.getConnection();
         PreparedStatement statement = connection.prepareStatement(
             "SELECT * FROM World WHERE id = ?",
             ResultSet.TYPE_FORWARD_ONLY,
             ResultSet.CONCUR_READ_ONLY)) {
      for (int i = 0; i < queries; i++) {
        statement.setInt(1, Helper.randomWorld());
        try (ResultSet resultSet = statement.executeQuery()) {
          resultSet.next();
          worlds[i] = new World(
              resultSet.getInt("id"),
              resultSet.getInt("randomNumber"));
        }
      }
    }
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, JSON_UTF8);
    if (queries == 1)
    {
      exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds[0]));
    }
    else
    {
      exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
    }
  }
}
