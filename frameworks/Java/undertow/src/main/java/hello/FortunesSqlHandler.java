package hello;

import static hello.Helper.sendHtml;

import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import javax.sql.DataSource;

/**
 * Handles the fortunes test using a SQL database.
 */
final class FortunesSqlHandler implements HttpHandler {
  private final DataSource db;

  FortunesSqlHandler(DataSource db) {
    this.db = Objects.requireNonNull(db);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) throws Exception {
    List<Fortune> fortunes = new ArrayList<>();
    try (Connection connection = db.getConnection();
         PreparedStatement statement =
             connection.prepareStatement("SELECT * FROM Fortune");
         ResultSet resultSet = statement.executeQuery()) {
      while (resultSet.next()) {
        int id = resultSet.getInt("id");
        String message = resultSet.getString("message");
        fortunes.add(new Fortune(id, message));
      }
    }
    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    fortunes.sort(null);
    sendHtml(exchange, fortunes, "hello/fortunes.mustache");
  }
}
