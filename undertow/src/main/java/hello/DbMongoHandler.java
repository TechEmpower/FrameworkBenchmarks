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

import static hello.HelloWebServer.JSON_UTF8;

/**
 * Handles the single- and multiple-query database tests using MongoDB.
 */
final class DbMongoHandler implements HttpHandler {
  private final ObjectMapper objectMapper;
  private final DB database;

  DbMongoHandler(ObjectMapper objectMapper, DB database) {
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
      DBObject object = database.getCollection("World").findOne(
          new BasicDBObject("_id", Helper.randomWorld()));
      worlds[i] = new World(
          //
          // The creation script for the Mongo database inserts these numbers as
          // JavaScript numbers, which resolve to Doubles in Java.
          //
          ((Number) object.get("_id")).intValue(),
          ((Number) object.get("randomNumber")).intValue());
    }
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, JSON_UTF8);
    if (queries == 1)
    {
      exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds[0]));
    }
    else
    {
      exchange.getResponseSender().send(objectMapper.writeValueAsString(worlds));
    }
  }
}
