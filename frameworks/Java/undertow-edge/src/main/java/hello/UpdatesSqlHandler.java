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
 * Handles the updates test using a SQL database.
 */
final class UpdatesSqlHandler implements HttpHandler {
  private final ObjectMapper objectMapper;
  private final DataSource database;

  UpdatesSqlHandler(ObjectMapper objectMapper, DataSource database) {
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
         PreparedStatement query = connection.prepareStatement(
             "SELECT * FROM World WHERE id = ?",
             ResultSet.TYPE_FORWARD_ONLY,
             ResultSet.CONCUR_READ_ONLY);
         PreparedStatement update = connection.prepareStatement(
             "UPDATE World SET randomNumber = ? WHERE id= ?")) {
      for (int i = 0; i < queries; i++) {
        query.setInt(1, Helper.randomWorld());
        World world;
        try (ResultSet resultSet = query.executeQuery()) {
          resultSet.next();
          world = new World(
              resultSet.getInt("id"),
              resultSet.getInt("randomNumber"));
        }
        world.randomNumber = Helper.randomWorld();
        update.setInt(1, world.randomNumber);
        update.setInt(2, world.id);
        update.executeUpdate();
        worlds[i] = world;
      }
    }
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, JSON_UTF8);
    exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
  }
}
