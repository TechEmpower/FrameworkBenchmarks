import org.vertx.java.core.Handler;
import org.vertx.java.core.buffer.Buffer;
import org.vertx.java.core.eventbus.Message;
import org.vertx.java.core.http.HttpServerRequest;
import org.vertx.java.core.http.HttpServerResponse;
import org.vertx.java.core.json.JsonArray;
import org.vertx.java.core.json.JsonObject;
import org.vertx.java.core.json.impl.Json;
import org.vertx.java.platform.Verticle;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Date;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

public class WebServer extends Verticle implements Handler<HttpServerRequest> {

  private Buffer helloWorldBuffer = new Buffer("Hello, World!");
  private String helloWorldContentLength = String.valueOf(helloWorldBuffer.length());
  private DateFormat DATE_FORMAT = new SimpleDateFormat("EEE, dd MMM yyyyy HH:mm:ss z");
  private String dateString;

  @Override
  public void start() {
    vertx.createHttpServer().requestHandler(WebServer.this).listen(8080);
    vertx.setPeriodic(1000, new Handler<Long>() {
      @Override
      public void handle(Long timerID) {
        formatDate();
      }
    });
    formatDate();
  }

  @Override
  public void handle(HttpServerRequest req) {
    String path = req.path();
    switch (path) {
      case "/plaintext":
        handlePlainText(req);
        break;
      case "/json":
        handleJson(req);
        break;
      case "/db":
        handleDbMongo(req);
        break;
      case "/queries":
        handleQueriesMongo(req);
        break;
      default:
        req.response().setStatusCode(404);
        req.response().end();
    }
  }

  private void formatDate() {
    dateString = DATE_FORMAT.format(new Date());
  }

  private void handlePlainText(HttpServerRequest req) {
    HttpServerResponse resp = req.response();
    setHeaders(resp, "text/plain", helloWorldContentLength);
    resp.end(helloWorldBuffer);
  }

  private void handleJson(HttpServerRequest req) {
    Buffer buff = new Buffer(Json.encode(Collections.singletonMap("message", "Hello, world!")));
    HttpServerResponse resp = req.response();
    setHeaders(resp, "application/json", String.valueOf(buff.length()));
    resp.end(buff);
  }

  private void handleDbMongo(final HttpServerRequest req) {
    findRandom(ThreadLocalRandom.current(), new Handler<Message<JsonObject>>() {
      @Override
      public void handle(Message<JsonObject> reply) {
        JsonObject world = getResultFromReply(reply);
        String result = world.encode();
        sendResponse(req, result);
      }
    });
  }

  private JsonObject getResultFromReply(Message<JsonObject> reply) {
    JsonObject body = reply.body();
    JsonObject world = body.getObject("result");
    Object id = world.removeField("_id");
    world.putValue("id", id);
    return world;
  }

  private void handleQueriesMongo(final HttpServerRequest req) {
    int queriesParam = 1;
    try {
      queriesParam = Integer.parseInt(req.params().get("queries"));
    } catch (NumberFormatException e) {
      e.printStackTrace();
    }
    final MongoHandler dbh = new MongoHandler(req, queriesParam);
    final Random random = ThreadLocalRandom.current();
    for (int i = 0; i < queriesParam; i++) {
      findRandom(random, dbh);
    }
  }

  private void findRandom(Random random, Handler<Message<JsonObject>> handler) {
    vertx.eventBus().send(
        "hello.persistor",
        new JsonObject()
            .putString("action", "findone")
            .putString("collection", "World")
            .putObject("matcher", new JsonObject().putNumber("_id", (random.nextInt(10000) + 1))),
        handler);
  }

  private void sendResponse(HttpServerRequest req, String result) {
    Buffer buff = new Buffer(result);
    HttpServerResponse resp = req.response();
    setHeaders(resp, "application/json", String.valueOf(buff.length()));
    resp.end(buff);
  }

  private void setHeaders(HttpServerResponse resp, String contentType, String contentLength) {
    resp.putHeader("Content-Type", contentType);
    resp.putHeader("Content-Length", contentLength);
    resp.putHeader("Server", "vert.x");
    resp.putHeader("Date", dateString);
  }

  private class MongoHandler implements Handler<Message<JsonObject>> {
    private final HttpServerRequest req;
    private final int queries;
    private final JsonArray worlds;

    public MongoHandler(HttpServerRequest request, int queriesParam) {
      req = request;
      queries = queriesParam;
      worlds = new JsonArray();
    }

    @Override
    public void handle(Message<JsonObject> reply) {
      JsonObject world = getResultFromReply(reply);
      worlds.add(world);
      if (worlds.size() == this.queries) {
        // All queries have completed; send the response.
        String result = worlds.encode();
        sendResponse(req, result);
      }
    }
  }

}

