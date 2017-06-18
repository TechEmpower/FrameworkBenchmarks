package hello;

import static hello.Helper.getQueries;
import static hello.Helper.randomWorld;
import static hello.Helper.sendJson;

import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import com.mongodb.client.model.Filters;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import org.bson.Document;

/**
 * Handles the multi-query database test using MongoDB.
 */
final class QueriesMongoHandler implements HttpHandler {
  private final MongoCollection<Document> worldCollection;

  QueriesMongoHandler(MongoDatabase db) {
    worldCollection = db.getCollection("world");
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) {
    int queries = getQueries(exchange);
    World[] worlds = new World[queries];
    for (int i = 0; i < worlds.length; i++) {
      worlds[i] =
          worldCollection
              .find(Filters.eq(randomWorld()))
              .map(Helper::mongoDocumentToWorld)
              .first();
    }
    sendJson(exchange, worlds);
  }
}
