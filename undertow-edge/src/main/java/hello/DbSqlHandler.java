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
  private final boolean multiple;

  DbSqlHandler(ObjectMapper objectMapper, DataSource database, boolean multiple) {
    this.objectMapper = Objects.requireNonNull(objectMapper);
    this.database = Objects.requireNonNull(database);
    this.multiple = multiple;
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) throws Exception {
    if (exchange.isInIoThread()) {
      exchange.dispatch(this);
      return;
    }
    
    int queries = 1;
    if(multiple)
    {
      queries = Helper.getQueries(exchange);
    }
    
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
    
    if (multiple)
    {
      // If a multiple query then response must be an array
      exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
    }
    else
    {
      // If a single query then response must be an object
      exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds[0]));
    }
  }
}
