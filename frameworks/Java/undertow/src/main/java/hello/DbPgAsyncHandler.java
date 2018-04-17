package hello;

import static hello.Helper.randomWorldNumber;
import static hello.Helper.sendException;
import static hello.Helper.sendJson;

import io.reactiverse.pgclient.PgClient;
import io.reactiverse.pgclient.PgResult;
import io.reactiverse.pgclient.Row;
import io.reactiverse.pgclient.Tuple;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.vertx.core.AsyncResult;
import java.util.Objects;

/**
 * Handles the single-query database test using PostgreSQL with an asynchronous
 * API.
 */
final class DbPgAsyncHandler implements HttpHandler {
  private final PgClient client;

  DbPgAsyncHandler(PgClient client) {
    this.client = Objects.requireNonNull(client);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) {
    client.preparedQuery(
        "SELECT id, randomnumber FROM World WHERE id = $1",
        Tuple.of(randomWorldNumber()),
        (AsyncResult<PgResult<Row>> result) -> {
          if (result.failed()) {
            sendException(exchange, result.cause());
          } else {
            Row row = result.result().iterator().next();
            int id = row.getInteger(0);
            int randomNumber = row.getInteger(1);
            World world = new World(id, randomNumber);
            sendJson(exchange, world);
          }
        });
  }
}
