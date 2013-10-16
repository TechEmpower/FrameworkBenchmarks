import java.nio.charset.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import org.vertx.java.core.Handler;
import org.vertx.java.core.buffer.Buffer;
import org.vertx.java.core.eventbus.Message;
import org.vertx.java.core.http.HttpServerRequest;
import org.vertx.java.core.json.JsonObject;
import org.vertx.java.core.json.impl.Json;
import org.vertx.java.platform.Verticle;

public class WebServer extends Verticle implements Handler<HttpServerRequest> {

  private static String helloWorld = "Hello, World!";
  private static Buffer helloWorldBuffer = new Buffer(helloWorld);
  private static String helloWorldContentLength = String.valueOf(helloWorldBuffer.length());

  @Override
  public void start() {
    vertx.createHttpServer().requestHandler(WebServer.this).listen(8080);
  }

  @Override
  public void handle(HttpServerRequest req) {
    String path = req.path();
    switch (path.charAt(1)) {
      case 'p':
        handlePlainText(req);
        break;
      case 'j':
        handleJson(req);
        break;
      case 'd':
        handleDbMongo(req);
        break;
      default:
        req.response().setStatusCode(404);
        req.response().end();
    }

  }

  private void handlePlainText(HttpServerRequest req) {
    req.response().putHeader("Content-Type", "text/plain; charset=UTF-8");
    req.response().putHeader("Content-Length", helloWorldContentLength);
    req.response().end(helloWorldBuffer);
  }

  private void handleJson(HttpServerRequest req) {
    String result = Json.encode(Collections.singletonMap("message", "Hello, world!"));
    int contentLength = result.getBytes(StandardCharsets.UTF_8).length;
    req.response().putHeader("Content-Type", "application/json; charset=UTF-8");
    req.response().putHeader("Content-Length", String.valueOf(contentLength));
    req.response().end(result);
  }

  private void handleDbMongo(final HttpServerRequest req) {
    int queriesParam = 1;
    try {
      queriesParam = Integer.parseInt(req.params().get("queries"));
    } catch (NumberFormatException e) {
      // do nothing
    }

    final MongoHandler dbh = new MongoHandler(req, queriesParam);
    final Random random = ThreadLocalRandom.current();

    for (int i = 0; i < queriesParam; i++) {
      vertx.eventBus().send(
          "hello.persistor",
          new JsonObject()
              .putString("action", "findone")
              .putString("collection", "world")
              .putObject("matcher", new JsonObject().putNumber("id", (random.nextInt(10000) + 1))),
          dbh);
    }
  }

  private static class MongoHandler implements Handler<Message<JsonObject>> {
    private final HttpServerRequest req;
    private final int queries;
    private final List<Object> worlds = new ArrayList<>();

    public MongoHandler(HttpServerRequest request, int queriesParam) {
      this.req = request;
      this.queries = queriesParam;
    }

    @Override
    public void handle(Message<JsonObject> reply) {
      final JsonObject body = reply.body();

      if ("ok".equals(body.getString("status"))) {
        this.worlds.add(body.getObject("result"));
        if (this.worlds.size() == this.queries) {
          // All queries have completed; send the response.
          final String result = Json.encode(worlds);
          final int contentLength = result.getBytes(StandardCharsets.UTF_8).length;
          this.req.response().putHeader("Content-Type", "application/json; charset=UTF-8");
          this.req.response().putHeader("Content-Length", String.valueOf(contentLength));
          this.req.response().end(result);
        }
      } else {
        System.err.println("Failed to execute query");
      }
    }
  }
}
