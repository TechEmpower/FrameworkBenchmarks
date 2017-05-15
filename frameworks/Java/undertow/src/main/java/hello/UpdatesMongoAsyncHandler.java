package hello;

import static hello.Helper.getQueries;
import static hello.Helper.randomWorld;
import static hello.Helper.sendException;
import static hello.Helper.sendJson;

import com.mongodb.async.client.MongoCollection;
import com.mongodb.async.client.MongoDatabase;
import com.mongodb.client.model.Filters;
import com.mongodb.client.model.UpdateOneModel;
import com.mongodb.client.model.Updates;
import com.mongodb.client.model.WriteModel;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import org.bson.Document;
import org.bson.conversions.Bson;

/**
 * Handles the updates test using MongoDB with an asynchronous API.
 */
final class UpdatesMongoAsyncHandler implements HttpHandler {
  private final MongoCollection<Document> worldCollection;

  UpdatesMongoAsyncHandler(MongoDatabase db) {
    worldCollection = db.getCollection("world");
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) {
    int queries = getQueries(exchange);
    nUpdatedWorlds(queries).whenComplete(
        (worlds, exception) -> {
          if (exception != null) {
            sendException(exchange, exception);
          } else {
            sendJson(exchange, worlds);
          }
        });
  }

  private CompletableFuture<World[]> nUpdatedWorlds(int n) {
    return nWorlds(n).thenCompose(
        worlds -> {
          List<WriteModel<Document>> writes = new ArrayList<>(worlds.length);
          for (World world : worlds) {
            world.randomNumber = randomWorld();
            Bson filter = Filters.eq(world.id);
            Bson update = Updates.set("randomNumber", world.randomNumber);
            writes.add(new UpdateOneModel<>(filter, update));
          }
          CompletableFuture<World[]> next = new CompletableFuture<>();
          worldCollection.bulkWrite(
              writes,
              (result, exception) -> {
                if (exception != null) {
                  next.completeExceptionally(exception);
                } else {
                  next.complete(worlds);
                }
              });
          return next;
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
