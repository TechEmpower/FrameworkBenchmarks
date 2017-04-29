package hello;

import static hello.Helper.sendException;
import static hello.Helper.sendHtml;

import com.mongodb.async.client.MongoCollection;
import com.mongodb.async.client.MongoDatabase;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import java.util.ArrayList;
import org.bson.Document;

/**
 * Handles the fortunes test using MongoDB with an asynchronous API.
 */
final class FortunesMongoAsyncHandler implements HttpHandler {
  private final MongoCollection<Document> fortuneCollection;

  FortunesMongoAsyncHandler(MongoDatabase db) {
    fortuneCollection = db.getCollection("fortune");
  }

  @Override
  public void handleRequest(HttpServerExchange exchange) {
    fortuneCollection
        .find()
        .map(Helper::mongoDocumentToFortune)
        .into(
            new ArrayList<>(),
            (fortunes, exception) -> {
              if (exception != null) {
                sendException(exchange, exception);
              } else {
                fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                fortunes.sort(null);
                sendHtml(exchange, fortunes, "hello/fortunes.mustache");
              }
            });
  }
}
