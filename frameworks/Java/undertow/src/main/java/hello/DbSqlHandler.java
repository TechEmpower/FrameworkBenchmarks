package hello;

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
 * Handles the single-query database test using a SQL database.
 */
final class DbSqlHandler implements HttpHandler {
  private final DataSource db;

  DbSqlHandler(DataSource db) {
    this.db = Objects.requireNonNull(db);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) throws Exception {
    World world;
    try (Connection connection = db.getConnection();
         PreparedStatement statement =
             connection.prepareStatement("SELECT * FROM World WHERE id = ?")) {
      statement.setInt(1, randomWorld());
      try (ResultSet resultSet = statement.executeQuery()) {
        resultSet.next();
        int id = resultSet.getInt("id");
        int randomNumber = resultSet.getInt("randomNumber");
        world = new World(id, randomNumber);
      }
    }
    sendJson(exchange, world);
  }
}
