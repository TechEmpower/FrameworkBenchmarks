import com.mongodb.BasicDBObject;
import com.mongodb.DB;
import com.mongodb.DBObject;
import com.mongodb.MongoClient;
import org.vertx.java.core.Handler;
import org.vertx.java.core.buffer.Buffer;
import org.vertx.java.core.http.HttpServerRequest;
import org.vertx.java.core.http.HttpServerResponse;
import org.vertx.java.core.json.impl.Json;
import org.vertx.java.platform.Verticle;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Date;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

public class WebServer extends Verticle implements Handler<HttpServerRequest> {

  private static String helloWorld = "Hello, World!";
  private static Buffer helloWorldBuffer = new Buffer(helloWorld);
  private static String helloWorldContentLength = String.valueOf(helloWorldBuffer.length());
  private static DateFormat DATE_FORMAT = new SimpleDateFormat("EEE, dd MMM yyyyy HH:mm:ss z");

  private String dateString;
  private DB mongodb;

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

    final String mongoHost = "localhost:27017";
    final String mongoDBName = "hello_world";
    try {
      mongodb = new MongoClient(mongoHost).getDB(mongoDBName);
    } catch (Exception e) {
      e.printStackTrace();
    }
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
        req.response().setStatusCode(404).end();
    }
  }

  private void handlePlainText(HttpServerRequest req) {
    req.response()
       .putHeader("Content-Type", "application/json; charset=UTF-8")
       .putHeader("Content-Length", helloWorldContentLength)
       .putHeader("Server", "vert.x")
       .putHeader("Date", dateString)
       .end(helloWorldBuffer);
  }

  private void handleJson(HttpServerRequest req) {
    sendResponse(req, Json.encode(Collections.singletonMap("message", "Hello, world!")));
  }

  private Entry getMongoEntry() {
    Random random = ThreadLocalRandom.current();
    int id = (random.nextInt(10000) + 1);
    DBObject doc = mongodb.getCollection("World").findOne(new BasicDBObject("_id", id));
    return new Entry(((Number)doc.get("_id")).intValue(),
                     ((Number)doc.get("randomNumber")).intValue());
  }

  private void handleDbMongo(final HttpServerRequest req) {
    sendResponse(req, Json.encode(getMongoEntry()));
  }

  private void handleQueriesMongo(final HttpServerRequest req) {
    int queriesParam = 1;
    try {
      queriesParam = Integer.parseInt(req.params().get("queries"));
    } catch (NumberFormatException e) {
      e.printStackTrace();
    }
    Entry[] entries = new Entry[queriesParam];
    for (int i = 0; i < queriesParam; i++) {
      entries[i] = getMongoEntry();
    }
    sendResponse(req, Json.encode(entries));
  }

  private void sendResponse(HttpServerRequest req, String result) {
    Buffer buff = new Buffer(result);
    HttpServerResponse resp = req.response();
    resp.putHeader("Content-Type", "application/json; charset=UTF-8")
        .putHeader("Content-Length", String.valueOf(buff.length()))
        .putHeader("Server", "vert.x")
        .putHeader("Date", dateString)
        .end(buff);
  }

  private static final class Entry {
    public int id;
    public int randomNumber;
    public Entry(int id, int randomNumber) {
      this.id = id;
      this.randomNumber = randomNumber;
    }
  }
}

