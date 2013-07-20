package hello;

import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;
import com.google.common.net.MediaType;
import com.mongodb.DB;
import com.mongodb.DBCursor;
import com.mongodb.DBObject;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Handles the fortunes test using MongoDB.
 */
final class FortunesMongoHandler implements HttpHandler {
  private final MustacheFactory mustacheFactory;
  private final DB database;

  FortunesMongoHandler(MustacheFactory mustacheFactory, DB database) {
    this.mustacheFactory = Objects.requireNonNull(mustacheFactory);
    this.database = Objects.requireNonNull(database);
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) throws Exception {
    if (exchange.isInIoThread()) {
      exchange.dispatch(this);
      return;
    }
    List<Fortune> fortunes = new ArrayList<>();
    DBCursor cursor = database.getCollection("fortune").find();
    while (cursor.hasNext()) {
      DBObject object = cursor.next();
      fortunes.add(new Fortune(
          ((Number) object.get("id")).intValue(),
          (String) object.get("message")));
    }
    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    Collections.sort(fortunes);
    Mustache mustache = mustacheFactory.compile("hello/fortunes.mustache");
    StringWriter writer = new StringWriter();
    mustache.execute(writer, fortunes);
    exchange.getResponseHeaders().put(
        Headers.CONTENT_TYPE, MediaType.HTML_UTF_8.toString());
    exchange.getResponseSender().send(writer.toString());
  }
}
