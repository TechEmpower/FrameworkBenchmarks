package hello;

import static hello.Helper.getQueries;
import static hello.Helper.randomWorld;
import static hello.Helper.sendException;
import static hello.Helper.sendJson;
import static hello.Helper.toCompletableFuture;

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
import java.util.stream.IntStream;
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
    IntStream
        .range(0, getQueries(exchange))
        .mapToObj(
            i -> {
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
            })
        .collect(toCompletableFuture())
        .thenCompose(
            worlds -> {
              List<WriteModel<Document>> writes = new ArrayList<>(worlds.size());
              for (World world : worlds) {
                world.randomNumber = randomWorld();
                Bson filter = Filters.eq(world.id);
                Bson update = Updates.set("randomNumber", world.randomNumber);
                writes.add(new UpdateOneModel<>(filter, update));
              }
              CompletableFuture<List<World>> next = new CompletableFuture<>();
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
            })
        .whenComplete(
            (worlds, exception) -> {
              if (exception != null) {
                sendException(exchange, exception);
              } else {
                sendJson(exchange, worlds);
              }
            });
  }
}
