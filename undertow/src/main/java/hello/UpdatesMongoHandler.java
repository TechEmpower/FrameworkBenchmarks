package hello;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.net.MediaType;
import com.mongodb.BasicDBObject;
import com.mongodb.DB;
import com.mongodb.DBObject;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

import java.util.Objects;

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
      DBObject key = new BasicDBObject("id", id);
      //
      // The requirements for the test dictate that we must fetch the World
      // object from the data store and read its randomNumber field, even though
      // we could technically avoid doing either of those things and still
      // produce the correct output and side effects.
      //
      DBObject object = database.getCollection("world").findOne(key);
      int oldRandomNumber = ((Number) object.get("randomNumber")).intValue();
      int newRandomNumber = Helper.randomWorld();
      object.put("randomNumber", newRandomNumber);
      database.getCollection("world").update(key, object);
      worlds[i] = new World(id, newRandomNumber);
    }
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, MediaType.JSON_UTF_8.toString());
    exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
  }
}
