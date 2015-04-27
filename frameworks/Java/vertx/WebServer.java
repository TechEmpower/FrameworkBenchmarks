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

  private final Buffer helloWorldBuffer = new Buffer("Hello, World!");
  private final String helloWorldContentLength = String.valueOf(helloWorldBuffer.length());
  private final DateFormat DATE_FORMAT = new SimpleDateFormat("EEE, dd MMM yyyyy HH:mm:ss z");
  private final Random random = ThreadLocalRandom.current();
  private String dateString;

  private static final String PATH_PLAINTEXT = "/plaintext";
  private static final String PATH_JSON = "/json";
  private static final String PATH_DB = "/db";
  private static final String PATH_QUERIES = "/queries";
  private static final String PATH_UPDATES = "/updates";
  private static final String RESPONSE_TYPE_PLAIN = "text/plain";
  private static final String RESPONSE_TYPE_JSON = "application/json";
  private static final String HEADER_CONTENT_TYPE = "Content-Type";
  private static final String HEADER_CONTENT_LENGTH = "Content-Length";
  private static final String HEADER_SERVER = "Server";
  private static final String HEADER_SERVER_VERTX = "vert.x";
  private static final String HEADER_DATE = "Date";
  private static final String MONGO_ADDRESS = "hello.persistor";
  private static final String UNDERSCORE_ID = "_id";
  private static final String TEXT_ID = "id";
  private static final String RANDOM_NUMBER = "randomNumber";
  private static final String TEXT_RESULT = "result";
  private static final String TEXT_QUERIES = "queries";
  private static final String TEXT_MESSAGE = "message";
  private static final String HELLO_WORLD = "Hello, world!";
  private static final String TEXT_ACTION = "action";
  private static final String TEXT_CRITERIA = "criteria";
  private static final String TEXT_UPDATE = "update";
  private static final String TEXT_OBJ_NEW = "objNew";
  private static final String TEXT_FINDONE = "findone";
  private static final String TEXT_COLLECTION = "collection";
  private static final String TEXT_WORLD = "World";
  private static final String TEXT_MATCHER = "matcher";

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
    switch (req.path()) {
      case PATH_PLAINTEXT:
        handlePlainText(req);
        break;
      case PATH_JSON:
        handleJson(req);
        break;
      case PATH_DB:
        handleDbMongo(req);
        break;
      case PATH_QUERIES:
        handleDBMongo(req,false);
        break;
      case PATH_UPDATES:
        handleDBMongo(req,true);
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
    setHeaders(resp, RESPONSE_TYPE_PLAIN, helloWorldContentLength);
    resp.end(helloWorldBuffer);
  }

  private void handleJson(HttpServerRequest req) {
    Buffer buff = new Buffer(Json.encode(Collections.singletonMap(TEXT_MESSAGE, HELLO_WORLD)));
    HttpServerResponse resp = req.response();
    setHeaders(resp, RESPONSE_TYPE_JSON, String.valueOf(buff.length()));
    resp.end(buff);
  }

  private void handleDbMongo(final HttpServerRequest req) {
    findRandom(new Handler<Message<JsonObject>>() {
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
    JsonObject world = body.getObject(TEXT_RESULT);
    Object id = world.removeField(UNDERSCORE_ID);
    if (id instanceof Double) {
      world.putValue(TEXT_ID, Integer.valueOf(((Double)id).intValue()));
    } else {
      world.putValue(TEXT_ID, id);
    }
    return world;
  }

  private void handleDBMongo(final HttpServerRequest req, boolean randomUpdates) {
    int queriesParam = 1;
    try {
      queriesParam = Integer.parseInt(req.params().get(TEXT_QUERIES));
    } catch (NumberFormatException e) {
      queriesParam = 1;
    }
    if (queriesParam < 1) {
      queriesParam = 1;
    } else if (queriesParam > 500) {
      queriesParam = 500;
    }
    final MongoHandler dbh = new MongoHandler(req, queriesParam, randomUpdates);
    for (int i = 0; i < queriesParam; i++) {
      findRandom(dbh);
    }
  }

  private void findRandom(Handler<Message<JsonObject>> handler) {
    vertx.eventBus().send(
        MONGO_ADDRESS,
        new JsonObject()
            .putString(TEXT_ACTION, TEXT_FINDONE)
            .putString(TEXT_COLLECTION, TEXT_WORLD)
            .putObject(TEXT_MATCHER, new JsonObject().putNumber(UNDERSCORE_ID, (random.nextInt(10000) + 1))),
        handler);
  }

  private void updateRandom(JsonObject json) {
    vertx.eventBus().send(
        MONGO_ADDRESS,
        new JsonObject()
            .putString(TEXT_ACTION, TEXT_UPDATE)
            .putString(TEXT_COLLECTION, TEXT_WORLD)
            .putObject(TEXT_CRITERIA, new JsonObject().putValue(UNDERSCORE_ID, json.getValue(TEXT_ID)))
            .putObject(TEXT_OBJ_NEW, json)
             );
  }

  private void sendResponse(HttpServerRequest req, String result) {
    Buffer buff = new Buffer(result);
    HttpServerResponse resp = req.response();
    setHeaders(resp, RESPONSE_TYPE_JSON, String.valueOf(buff.length()));
    resp.end(buff);
  }

  private void setHeaders(HttpServerResponse resp, String contentType, String contentLength) {
    resp.putHeader(HEADER_CONTENT_TYPE, contentType);
    resp.putHeader(HEADER_CONTENT_LENGTH, contentLength);
    resp.putHeader(HEADER_SERVER, HEADER_SERVER_VERTX );
    resp.putHeader(HEADER_DATE, dateString);
  }

  private class MongoHandler implements Handler<Message<JsonObject>> {
    private final HttpServerRequest req;
    private final int queries;
    private final JsonArray worlds;
    private final Random random;
    private final boolean randomUpdates;

    public MongoHandler(HttpServerRequest request, int queriesParam, boolean performRandomUpdates) {
      req = request;
      queries = queriesParam;
      randomUpdates = performRandomUpdates;
      random = ThreadLocalRandom.current();
      worlds = new JsonArray();
    }

    @Override
    public void handle(Message<JsonObject> reply) {
      JsonObject world = getResultFromReply(reply);
      if (randomUpdates) {
        world.putValue(RANDOM_NUMBER, (random.nextInt(10000) + 1));
        updateRandom(world);        
      }
      worlds.add(world);
      if (worlds.size() == this.queries) {
        // All queries have completed; send the response.
        String result = worlds.encode();
        sendResponse(req, result);
      }
    }
  }

}

