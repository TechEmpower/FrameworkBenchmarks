package vertx;

import com.julienviet.pgclient.*;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Handler;
import io.vertx.core.MultiMap;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.core.http.HttpServerRequest;
import io.vertx.core.http.HttpServerResponse;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import vertx.model.Fortune;
import vertx.model.Message;
import vertx.model.World;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public class App extends AbstractVerticle implements Handler<HttpServerRequest> {

  /**
   * Returns the value of the "queries" getRequest parameter, which is an integer
   * bound between 1 and 500 with a default value of 1.
   *
   * @param request the current HTTP request
   * @return the value of the "queries" parameter
   */
  static int getQueries(HttpServerRequest request) {
    String param = request.getParam("queries");

    if (param == null) {
      return 1;
    }
    try {
      int parsedValue = Integer.parseInt(param);
      return Math.min(500, Math.max(1, parsedValue));
    } catch (NumberFormatException e) {
      return 1;
    }
  }

  static Logger logger = LoggerFactory.getLogger(App.class.getName());

  private static final String PATH_PLAINTEXT = "/plaintext";
  private static final String PATH_JSON = "/json";
  private static final String PATH_DB = "/db";
  private static final String PATH_QUERIES = "/queries";
  private static final String PATH_UPDATES = "/updates";
  private static final String PATH_FORTUNES = "/fortunes";

  private static final CharSequence RESPONSE_TYPE_PLAIN = HttpHeaders.createOptimized("text/plain");
  private static final CharSequence RESPONSE_TYPE_HTML = HttpHeaders.createOptimized("text/html; charset=UTF-8");
  private static final CharSequence RESPONSE_TYPE_JSON = HttpHeaders.createOptimized("application/json");

  private static final String HELLO_WORLD = "Hello, world!";
  private static final Buffer HELLO_WORLD_BUFFER = Buffer.buffer(HELLO_WORLD);

  private static final CharSequence HEADER_SERVER = HttpHeaders.createOptimized("server");
  private static final CharSequence HEADER_DATE = HttpHeaders.createOptimized("date");
  private static final CharSequence HEADER_CONTENT_TYPE = HttpHeaders.createOptimized("content-type");
  private static final CharSequence HEADER_CONTENT_LENGTH = HttpHeaders.createOptimized("content-length");

  private static final CharSequence HELLO_WORLD_LENGTH = HttpHeaders.createOptimized("" + HELLO_WORLD.length());
  private static final CharSequence SERVER = HttpHeaders.createOptimized("vert.x");

  private static final String UPDATE_WORLD = "UPDATE world SET randomnumber=$1 WHERE id=$2";
  private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
  private static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";

  private CharSequence dateString;

  private HttpServer server;

  private PgClient client;

  @Override
  public void start() throws Exception {
    int port = 8080;
    server = vertx.createHttpServer(new HttpServerOptions());
    server.requestHandler(App.this).listen(port);
    dateString = HttpHeaders.createOptimized(java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now()));
    JsonObject config = config();
    vertx.setPeriodic(1000, handler -> {
      dateString = HttpHeaders.createOptimized(java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now()));
    });
    PgPoolOptions options = new PgPoolOptions();
    options.setDatabase(config.getString("database"));
    options.setHost(config.getString("host"));
    options.setPort(config.getInteger("port", 5432));
    options.setUsername(config.getString("username"));
    options.setPassword(config.getString("password"));
    options.setCachePreparedStatements(true);
    options.setMaxSize(1);
    client = PgClient.pool(vertx, options);
  }

  @Override
  public void handle(HttpServerRequest request) {
    switch (request.path()) {
      case PATH_PLAINTEXT:
        handlePlainText(request);
        break;
      case PATH_JSON:
        handleJson(request);
        break;
      case PATH_DB:
        handleDb(request);
        break;
      case PATH_QUERIES:
        new Queries().handle(request);
        break;
      case PATH_UPDATES:
        new Update(request).handle();
        break;
      case PATH_FORTUNES:
        handleFortunes(request);
        break;
      default:
        request.response().setStatusCode(404);
        request.response().end();
        break;
    }
  }

  @Override
  public void stop() {
    if (server != null) server.close();
  }

  private void handlePlainText(HttpServerRequest request) {
    HttpServerResponse response = request.response();
    MultiMap headers = response.headers();
    headers
        .add(HEADER_CONTENT_TYPE, RESPONSE_TYPE_PLAIN)
        .add(HEADER_SERVER, SERVER)
        .add(HEADER_DATE, dateString)
        .add(HEADER_CONTENT_LENGTH, HELLO_WORLD_LENGTH);
    response.end(HELLO_WORLD_BUFFER);
  }

  private void handleJson(HttpServerRequest request) {
    HttpServerResponse response = request.response();
    MultiMap headers = response.headers();
    headers
        .add(HEADER_CONTENT_TYPE, RESPONSE_TYPE_JSON)
        .add(HEADER_SERVER, SERVER)
        .add(HEADER_DATE, dateString);
    response.end(new Message("Hello, World!").toBuffer());
  }

  /**
   * Returns a random integer that is a suitable value for both the {@code id}
   * and {@code randomNumber} properties of a world object.
   *
   * @return a random world number
   */
  private static int randomWorld() {
    return 1 + ThreadLocalRandom.current().nextInt(10000);
  }

  private void handleDb(HttpServerRequest req) {
    HttpServerResponse resp = req.response();
    client.preparedQuery(SELECT_WORLD, Tuple.of(randomWorld()), res -> {
      if (res.succeeded()) {
        PgIterator<Row> resultSet = res.result().iterator();
        if (!resultSet.hasNext()) {
          resp.setStatusCode(404).end();
          return;
        }
        Tuple row = resultSet.next();
        resp
            .putHeader(HttpHeaders.SERVER, SERVER)
            .putHeader(HttpHeaders.DATE, dateString)
            .putHeader(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_JSON)
            .end(Json.encode(new World(row.getInteger(0), row.getInteger(1))));
      } else {
        logger.error(res.cause());
        resp.setStatusCode(500).end(res.cause().getMessage());
      }
    });
  }

  class Queries {

    boolean failed;
    JsonArray worlds = new JsonArray();

    private void handle(HttpServerRequest req) {
      HttpServerResponse resp = req.response();
      final int queries = getQueries(req);
      for (int i = 0; i < queries; i++) {
        client.preparedQuery(SELECT_WORLD, Tuple.of(randomWorld()), ar -> {
          if (!failed) {
            if (ar.failed()) {
              failed = true;
              resp.setStatusCode(500).end(ar.cause().getMessage());
              return;
            }

            // we need a final reference
            final Tuple row = ar.result().iterator().next();
            worlds.add(new JsonObject().put("id", "" + row.getInteger(0)).put("randomNumber", "" + row.getInteger(1)));

            // stop condition
            if (worlds.size() == queries) {
              resp
                  .putHeader(HttpHeaders.SERVER, SERVER)
                  .putHeader(HttpHeaders.DATE, dateString)
                  .putHeader(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_JSON)
                  .end(worlds.encode());
            }
          }
        });
      }
    }
  }

  class Update {

    final HttpServerRequest req;
    boolean failed;
    int queryCount;
    final World[] worlds;

    public Update(HttpServerRequest req) {
      final int queries = getQueries(req);
      this.req = req;
      this.worlds = new World[queries];
    }

    private void handle() {
      for (int i = 0; i < worlds.length; i++) {
        int id = randomWorld();
        int index = i;
        client.preparedQuery(SELECT_WORLD, Tuple.of(id), ar -> {
          if (!failed) {
            if (ar.failed()) {
              failed = true;
              sendError(ar.cause());
              return;
            }
            worlds[index] = new World(ar.result().iterator().next().getInteger(0), randomWorld());
            if (++queryCount == worlds.length) {
              handleUpdates();
            }
          }
        });
      }
    }

    void handleUpdates() {
      Arrays.sort(worlds);
      List<Tuple> batch = new ArrayList<>();
      for (World world : worlds) {
        batch.add(Tuple.of(world.getRandomNumber(), world.getId()));
      }
      client.preparedBatch(UPDATE_WORLD, batch, ar -> {
        if (ar.failed()) {
          sendError(ar.cause());
          return;
        }
        JsonArray json = new JsonArray();
        for (World world : worlds) {
          json.add(new JsonObject().put("id", "" + world.getId()).put("randomNumber", "" + world.getRandomNumber()));
        }
        req.response()
            .putHeader(HttpHeaders.SERVER, SERVER)
            .putHeader(HttpHeaders.DATE, dateString)
            .putHeader(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_JSON)
            .end(json.toBuffer());
      });
    }

    void sendError(Throwable err) {
      logger.error("", err);
      req.response().setStatusCode(500).end(err.getMessage());
    }
  }

  private void handleFortunes(HttpServerRequest req) {
    client.preparedQuery(SELECT_FORTUNE, ar -> {
      HttpServerResponse response = req.response();
      if (ar.succeeded()) {
        List<Fortune> fortunes = new ArrayList<>();
        PgIterator<Row> resultSet = ar.result().iterator();
        if (!resultSet.hasNext()) {
          response.setStatusCode(404).end("No results");
          return;
        }
        while (resultSet.hasNext()) {
          Tuple row = resultSet.next();
          fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
        }
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        response
            .putHeader(HttpHeaders.SERVER, SERVER)
            .putHeader(HttpHeaders.DATE, dateString)
            .putHeader(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_HTML)
            .end(FortunesTemplate.template(fortunes).render().toString());
      } else {
        Throwable err = ar.cause();
        logger.error("", err);
        response.setStatusCode(500).end(err.getMessage());
      }
    });
  }

  public static void main(String[] args) throws Exception {
    JsonObject config = new JsonObject(new String(Files.readAllBytes(new File(args[0]).toPath())));
    int procs = Runtime.getRuntime().availableProcessors();
    Vertx vertx = Vertx.vertx();
    vertx.exceptionHandler(err -> {
      err.printStackTrace();
    });
    vertx.deployVerticle(App.class.getName(),
        new DeploymentOptions().setInstances(procs * 2).setConfig(config), event -> {
          if (event.succeeded()) {
            logger.debug("Your Vert.x application is started!");
          } else {
            logger.error("Unable to start your application", event.cause());
          }
        });
  }
}
