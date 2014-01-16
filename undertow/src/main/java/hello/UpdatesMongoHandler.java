package hello;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mongodb.BasicDBObject;
import com.mongodb.DB;
import com.mongodb.DBObject;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

import java.util.Objects;

import static hello.HelloWebServer.JSON_UTF8;

/**
 * Handles the updates test using MongoDB.
 */
final class UpdatesMongoHandler implements HttpHandler {
  private final ObjectMapper objectMapper;
  private final DB database;

  UpdatesMongoHandler(ObjectMapper objectMapper, DB database) {
    this.objectMapper = Objects.requireNonNull(objectMapper);
    this.database = Objects.requireNonNull(database);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) throws Exception {
    if (exchange.isInIoThread()) {
      exchange.dispatch(this);
      return;
    }
    int queries = Helper.getQueries(exchange);
    World[] worlds = new World[queries];
    for (int i = 0; i < queries; i++) {
      int id = Helper.randomWorld();
      DBObject key = new BasicDBObject("_id", id);
      //
      // The requirements for the test dictate that we must fetch the World
      // object from the data store and read its randomNumber field, even though
      // we could technically avoid doing either of those things and still
      // produce the correct output and side effects.
      //
      DBObject object = database.getCollection("World").findOne(key);
      int newRandomNumber = Helper.randomWorld();
      object.put("randomNumber", newRandomNumber);
      database.getCollection("World").update(key, object);
      worlds[i] = new World(id, newRandomNumber);
    }
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, JSON_UTF8);
    exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
  }
}
