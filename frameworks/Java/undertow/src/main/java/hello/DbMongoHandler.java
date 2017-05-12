package hello;

import static hello.Helper.randomWorld;
import static hello.Helper.sendJson;

import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import com.mongodb.client.model.Filters;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import org.bson.Document;

/**
 * Handles the single-query database test using MongoDB.
 */
final class DbMongoHandler implements HttpHandler {
  private final MongoCollection<Document> worldCollection;

  DbMongoHandler(MongoDatabase db) {
    worldCollection = db.getCollection("world");
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) {
    World world =
        worldCollection
            .find(Filters.eq(randomWorld()))
            .map(Helper::mongoDocumentToWorld)
            .first();
    sendJson(exchange, world);
  }
}
