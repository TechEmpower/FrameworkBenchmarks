package hello;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

import javax.sql.DataSource;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Future;

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
    try (final Connection connection = database.getConnection()) {
      Map<Integer, Future<World>> futureWorlds = new ConcurrentHashMap<>();
      for (int i = 0; i < queries; i++) {
        futureWorlds.put(i, Helper.EXECUTOR.submit(new Callable<World>(){
          @Override
          public World call() throws Exception {
            try (PreparedStatement statement = connection.prepareStatement(
                "SELECT * FROM World WHERE id = ?",
                ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {

              statement.setInt(1, Helper.randomWorld());
              ResultSet resultSet = statement.executeQuery();
              resultSet.next();
              return new World(
                resultSet.getInt("id"),
                resultSet.getInt("randomNumber"));
            }
          }
        }));
      }

      for (int i = 0; i < queries; i++) {
        worlds[i] = futureWorlds.get(i).get();
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
