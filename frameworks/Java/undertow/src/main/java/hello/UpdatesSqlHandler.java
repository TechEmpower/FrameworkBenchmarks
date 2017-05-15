package hello;

import static hello.Helper.getQueries;
import static hello.Helper.randomWorld;
import static hello.Helper.sendJson;

import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.Objects;
import javax.sql.DataSource;

/**
 * Handles the updates test using a SQL database.
 */
final class UpdatesSqlHandler implements HttpHandler {
  private final DataSource db;

  UpdatesSqlHandler(DataSource db) {
    this.db = Objects.requireNonNull(db);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) throws Exception {
    int queries = getQueries(exchange);
    World[] worlds = new World[queries];
    try (Connection connection = db.getConnection()) {
      try (PreparedStatement statement =
               connection.prepareStatement(
                   "SELECT * FROM World WHERE id = ?")) {
        for (int i = 0; i < worlds.length; i++) {
          statement.setInt(1, randomWorld());
          try (ResultSet resultSet = statement.executeQuery()) {
            resultSet.next();
            int id = resultSet.getInt("id");
            int randomNumber = resultSet.getInt("randomNumber");
            worlds[i] = new World(id, randomNumber);
          }
        }
      }
      try (PreparedStatement statement =
               connection.prepareStatement(
                   "UPDATE World SET randomNumber = ? WHERE id = ?")) {
        for (World world : worlds) {
          world.randomNumber = randomWorld();
          statement.setInt(1, world.randomNumber);
          statement.setInt(2, world.id);
          statement.executeUpdate();
        }
      }
    }
    sendJson(exchange, worlds);
  }
}
