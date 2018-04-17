package hello;

import static hello.Helper.getQueries;
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
import java.util.concurrent.CompletableFuture;

/**
 * Handles the multi-query database test using PostgreSQL with an asynchronous
 * API.
 */
final class QueriesPgAsyncHandler implements HttpHandler {
  private final PgClient client;

  QueriesPgAsyncHandler(PgClient client) {
    this.client = Objects.requireNonNull(client);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) {
    int queries = getQueries(exchange);
    nWorlds(queries).whenComplete(
        (worlds, exception) -> {
          if (exception != null) {
            sendException(exchange, exception);
          } else {
            sendJson(exchange, worlds);
          }
        });
  }

  private CompletableFuture<World[]> nWorlds(int n) {
    @SuppressWarnings({ "unchecked", "rawtypes" })
    CompletableFuture<World>[] futures = new CompletableFuture[n];
    for (int i = 0; i < futures.length; i++) {
      futures[i] = oneWorld();
    }
    return CompletableFuture.allOf(futures).thenApply(
        nil -> {
          World[] worlds = new World[futures.length];
          for (int i = 0; i < futures.length; i++) {
            worlds[i] = futures[i].join();
          }
          return worlds;
        });
  }

  private CompletableFuture<World> oneWorld() {
    CompletableFuture<World> future = new CompletableFuture<>();
    client.preparedQuery(
        "SELECT id, randomnumber FROM World WHERE id = $1",
        Tuple.of(randomWorldNumber()),
        (AsyncResult<PgResult<Row>> result) -> {
          if (result.failed()) {
            future.completeExceptionally(result.cause());
          } else {
            Row row = result.result().iterator().next();
            int id = row.getInteger(0);
            int randomNumber = row.getInteger(1);
            World world = new World(id, randomNumber);
            future.complete(world);
          }
        });
    return future;
  }
}
