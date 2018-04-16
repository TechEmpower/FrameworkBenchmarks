package hello;

import static hello.Helper.sendException;
import static hello.Helper.sendHtml;

import io.reactiverse.pgclient.PgClient;
import io.reactiverse.pgclient.PgResult;
import io.reactiverse.pgclient.Row;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.vertx.core.AsyncResult;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Handles the fortunes test using PostgreSQL with an asynchronous API.
 */
final class FortunesPgAsyncHandler implements HttpHandler {
  private final PgClient client;

  FortunesPgAsyncHandler(PgClient client) {
    this.client = Objects.requireNonNull(client);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) {
    client.preparedQuery(
        "SELECT id, message FROM Fortune",
        (AsyncResult<PgResult<Row>> result) -> {
          if (result.failed()) {
            sendException(exchange, result.cause());
          } else {
            List<Fortune> fortunes = new ArrayList<>();
            for (Row row : result.result()) {
              int id = row.getInteger(0);
              String message = row.getString(1);
              fortunes.add(new Fortune(id, message));
            }
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            fortunes.sort(null);
            sendHtml(exchange, fortunes, "hello/fortunes.mustache");
          }
        });
  }
}
