package hello;

import static hello.Helper.randomWorld;
import static hello.Helper.sendException;
import static hello.Helper.sendJson;

import com.mongodb.async.client.MongoCollection;
import com.mongodb.async.client.MongoDatabase;
import com.mongodb.client.model.Filters;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import org.bson.Document;

/**
 * Handles the single-query database test using MongoDB with an asynchronous
 * API.
 */
final class DbMongoAsyncHandler implements HttpHandler {
  private final MongoCollection<Document> worldCollection;

  DbMongoAsyncHandler(MongoDatabase db) {
    worldCollection = db.getCollection("world");
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) {
    worldCollection
        .find(Filters.eq(randomWorld()))
        .map(Helper::mongoDocumentToWorld)
        .first(
            (world, exception) -> {
              if (exception != null) {
                sendException(exchange, exception);
              } else {
                sendJson(exchange, world);
              }
            });
  }
}
