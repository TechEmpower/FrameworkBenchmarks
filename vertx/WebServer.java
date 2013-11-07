import java.nio.charset.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import org.vertx.java.core.Handler;
import org.vertx.java.core.buffer.Buffer;
import org.vertx.java.core.eventbus.Message;
import org.vertx.java.core.http.HttpServerRequest;
import org.vertx.java.core.http.HttpServerResponse;
import org.vertx.java.core.json.JsonObject;
import org.vertx.java.core.json.JsonArray;
import org.vertx.java.core.json.impl.Json;
import org.vertx.java.platform.Verticle;

public class WebServer extends Verticle implements Handler<HttpServerRequest> {

  private static String helloWorld = "Hello, World!";
  private static Buffer helloWorldBuffer = new Buffer(helloWorld);
  private static String helloWorldContentLength = String.valueOf(helloWorldBuffer.length());
  private static DateFormat DATE_FORMAT = new SimpleDateFormat("EEE, dd MMM yyyyy HH:mm:ss z");

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

  private void formatDate() {
    dateString = DATE_FORMAT.format(new Date());
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

  private void handlePlainText(HttpServerRequest req) {
    HttpServerResponse resp = req.response();
    resp.putHeader("Content-Type", "application/json; charset=UTF-8");
    resp.putHeader("Content-Length", helloWorldContentLength);
    resp.putHeader("Server", "vert.x");
    resp.putHeader("Date", dateString);
    resp.end(helloWorldBuffer);
  }

  private void handleJson(HttpServerRequest req) {
    String result = Json.encode(Collections.singletonMap("message", "Hello, world!"));
    sendResponse(req, result);
  }

  private void handleDbMongo(final HttpServerRequest req) {

    final Random random = ThreadLocalRandom.current();

    vertx.eventBus().send(
        "hello.persistor",
        new JsonObject()
            .putString("action", "findone")
            .putString("collection", "world")
            .putObject("matcher", new JsonObject().putNumber("id", (random.nextInt(10000) + 1))),
        new Handler<Message<JsonObject>>() {
          @Override
          public void handle(Message<JsonObject> reply) {
            JsonObject body = reply.body();

            if ("ok".equals(body.getString("status"))) {
              JsonObject world = body.getObject("result");
              world.removeField("_id");
              String result = world.encode();
              sendResponse(req, result);
            } else {
              System.err.println("Failed to execute query");
            }
          }
        });
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
      vertx.eventBus().send(
          "hello.persistor",
          new JsonObject()
              .putString("action", "findone")
              .putString("collection", "world")
              .putObject("matcher", new JsonObject().putNumber("id", (random.nextInt(10000) + 1))),
          dbh);
    }
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
      JsonObject body = reply.body();

      if ("ok".equals(body.getString("status"))) {
        body.getObject("result").removeField("_id");
        worlds.add(body.getObject("result"));
        if (worlds.size() == this.queries) {
          // All queries have completed; send the response.
          String result = worlds.encode();
          sendResponse(req, result);
        }
      } else {
        System.err.println("Failed to execute query");
      }
    }
  }
  
  private void sendResponse(HttpServerRequest req, String result) {
      int contentLength = result.getBytes(StandardCharsets.UTF_8).length;
      HttpServerResponse resp = req.response();
      resp.putHeader("Content-Type", "application/json; charset=UTF-8");
      resp.putHeader("Content-Length", String.valueOf(contentLength));
      resp.putHeader("Server", "vert.x");
      resp.putHeader("Date", dateString);
      resp.end(result);
  }
}

