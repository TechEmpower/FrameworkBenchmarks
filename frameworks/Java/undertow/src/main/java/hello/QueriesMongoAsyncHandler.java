package hello;

import static hello.Helper.getQueries;
import static hello.Helper.randomWorld;
import static hello.Helper.sendException;
import static hello.Helper.sendJson;

import com.mongodb.async.client.MongoCollection;
import com.mongodb.async.client.MongoDatabase;
import com.mongodb.client.model.Filters;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import java.util.concurrent.CompletableFuture;
import org.bson.Document;

/**
 * Handles the multi-query database test using MongoDB with an asynchronous API.
 */
final class QueriesMongoAsyncHandler implements HttpHandler {
  private final MongoCollection<Document> worldCollection;

  QueriesMongoAsyncHandler(MongoDatabase db) {
    worldCollection = db.getCollection("world");
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
    @SuppressWarnings("unchecked")
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
    worldCollection
        .find(Filters.eq(randomWorld()))
        .map(Helper::mongoDocumentToWorld)
        .first(
            (world, exception) -> {
              if (exception != null) {
                future.completeExceptionally(exception);
              } else {
                future.complete(world);
              }
            });
    return future;
  }
}
