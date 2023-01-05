package vertx;

import com.fizzed.rocker.ContentType;
import com.fizzed.rocker.RockerOutputFactory;
import io.netty.util.concurrent.MultithreadEventExecutorGroup;
import io.vertx.pgclient.*;
import io.vertx.core.*;
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
import io.vertx.sqlclient.PreparedQuery;
import io.vertx.sqlclient.PreparedStatement;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;
import io.vertx.sqlclient.impl.SqlClientInternal;
import vertx.model.CachedWorld;
import vertx.model.Fortune;
import vertx.model.Message;
import vertx.model.World;
import vertx.model.WorldCache;
import vertx.rocker.BufferRockerOutput;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

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
  private static final String PATH_CACHING = "/cached-queries";

  private static final Handler<AsyncResult<Void>> NULL_HANDLER = null;

  private static final CharSequence RESPONSE_TYPE_PLAIN = HttpHeaders.createOptimized("text/plain");
  private static final CharSequence RESPONSE_TYPE_HTML = HttpHeaders.createOptimized("text/html; charset=UTF-8");
  private static final CharSequence RESPONSE_TYPE_JSON = HttpHeaders.createOptimized("application/json");

  private static final String HELLO_WORLD = "Hello, world!";
  private static final Buffer HELLO_WORLD_BUFFER = Buffer.buffer(HELLO_WORLD, "UTF-8");

  private static final CharSequence HEADER_SERVER = HttpHeaders.createOptimized("server");
  private static final CharSequence HEADER_DATE = HttpHeaders.createOptimized("date");
  private static final CharSequence HEADER_CONTENT_TYPE = HttpHeaders.createOptimized("content-type");
  private static final CharSequence HEADER_CONTENT_LENGTH = HttpHeaders.createOptimized("content-length");

  private static final CharSequence HELLO_WORLD_LENGTH = HttpHeaders.createOptimized("" + HELLO_WORLD.length());
  private static final CharSequence SERVER = HttpHeaders.createOptimized("vert.x");

  private static final String UPDATE_WORLD = "UPDATE world SET randomnumber=$1 WHERE id=$2";
  private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
  private static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";
  private static final String SELECT_WORLDS = "SELECT id, randomnumber from WORLD";

  private HttpServer server;

  private SqlClientInternal client;

  private CharSequence dateString;

  private CharSequence[] plaintextHeaders;

  private final RockerOutputFactory<BufferRockerOutput> factory = BufferRockerOutput.factory(ContentType.RAW);

  private PreparedQuery<RowSet<Row>> SELECT_WORLD_QUERY;
  private PreparedQuery<RowSet<Row>> SELECT_FORTUNE_QUERY;
  private PreparedQuery<RowSet<Row>> UPDATE_WORLD_QUERY;
  private WorldCache WORLD_CACHE;

  public static CharSequence createDateHeader() {
    return HttpHeaders.createOptimized(DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
  }

  @Override
  public void start(Promise<Void> startPromise) throws Exception {
    int port = 8080;
    server = vertx.createHttpServer(new HttpServerOptions());
    server.requestHandler(App.this).listen(port);
    dateString = createDateHeader();
    plaintextHeaders = new CharSequence[] {
        HEADER_CONTENT_TYPE, RESPONSE_TYPE_PLAIN,
        HEADER_SERVER, SERVER,
        HEADER_DATE, dateString,
        HEADER_CONTENT_LENGTH, HELLO_WORLD_LENGTH };
    JsonObject config = config();
    vertx.setPeriodic(1000, id -> plaintextHeaders[5] = dateString = createDateHeader());
    PgConnectOptions options = new PgConnectOptions();
    options.setDatabase(config.getString("database", "hello_world"));
    options.setHost(config.getString("host", "tfb-database"));
    options.setPort(config.getInteger("port", 5432));
    options.setUser(config.getString("username", "benchmarkdbuser"));
    options.setPassword(config.getString("password", "benchmarkdbpass"));
    options.setCachePreparedStatements(true);
    options.setPipeliningLimit(100_000); // Large pipelining means less flushing and we use a single connection anyway
    PgConnection.connect(vertx, options).flatMap(conn -> {
      client = (SqlClientInternal)conn;
      Future<PreparedStatement> f1 = conn.prepare(SELECT_WORLD);
      Future<PreparedStatement> f2 = conn.prepare(SELECT_FORTUNE);
      Future<PreparedStatement> f3 = conn.prepare(UPDATE_WORLD);
      Future<WorldCache> f4 = conn.preparedQuery(SELECT_WORLDS)
          .collecting(Collectors.mapping(row -> new CachedWorld(row.getInteger(0), row.getInteger(1)), Collectors.toList()))
          .execute().map(worlds -> new WorldCache(worlds.value()));
      f1.onSuccess(ps -> SELECT_WORLD_QUERY = ps.query());
      f2.onSuccess(ps -> SELECT_FORTUNE_QUERY = ps.query());
      f3.onSuccess(ps -> UPDATE_WORLD_QUERY = ps.query());
      f4.onSuccess(wc -> WORLD_CACHE = wc);
      return CompositeFuture.all(f1, f2, f3, f4);
    }).onComplete(ar -> startPromise.complete());
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
        new Queries(request).handle();
        break;
      case PATH_UPDATES:
        new Update(request).handle();
        break;
      case PATH_FORTUNES:
        handleFortunes(request);
        break;
      case PATH_CACHING:
        handleCaching(request);
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
    for (int i = 0;i < plaintextHeaders.length; i+= 2) {
      headers.add(plaintextHeaders[i], plaintextHeaders[i + 1]);
    }
    response.end(HELLO_WORLD_BUFFER, NULL_HANDLER);
  }

  private void handleJson(HttpServerRequest request) {
    HttpServerResponse response = request.response();
    MultiMap headers = response.headers();
    headers
        .add(HEADER_CONTENT_TYPE, RESPONSE_TYPE_JSON)
        .add(HEADER_SERVER, SERVER)
        .add(HEADER_DATE, dateString);
    response.end(new Message("Hello, World!").toBuffer(), NULL_HANDLER);
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
    SELECT_WORLD_QUERY.execute(Tuple.of(randomWorld()), res -> {
      if (res.succeeded()) {
        RowIterator<Row> resultSet = res.result().iterator();
        if (!resultSet.hasNext()) {
          resp.setStatusCode(404).end();
          return;
        }
        Row row = resultSet.next();
        resp
            .putHeader(HttpHeaders.SERVER, SERVER)
            .putHeader(HttpHeaders.DATE, dateString)
            .putHeader(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_JSON)
            .end(Json.encode(new World(row.getInteger(0), row.getInteger(1))), NULL_HANDLER);
      } else {
        logger.error(res.cause());
        resp.setStatusCode(500).end(res.cause().getMessage());
      }
    });
  }


  class Queries implements Handler<AsyncResult<RowSet<Row>>> {

    boolean failed;
    JsonArray worlds = new JsonArray();
    final HttpServerRequest req;
    final HttpServerResponse resp;
    final int queries;

    public Queries(HttpServerRequest req) {
      this.req = req;
      this.resp = req.response();
      this.queries = getQueries(req);
    }

    private void handle() {
      client.group(c -> {
        for (int i = 0; i < queries; i++) {
          c.preparedQuery(SELECT_WORLD).execute(Tuple.of(randomWorld()), this);
        }
      });
    }

    @Override
    public void handle(AsyncResult<RowSet<Row>> ar) {
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
              .end(worlds.encode(), NULL_HANDLER);
        }
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

      client.group(c -> {
        PreparedQuery<RowSet<Row>> preparedQuery = c.preparedQuery(SELECT_WORLD);
        for (int i = 0; i < worlds.length; i++) {
          int id = randomWorld();
          int index = i;
          preparedQuery.execute(Tuple.of(id), ar2 -> {
            if (!failed) {
              if (ar2.failed()) {
                failed = true;
                sendError(ar2.cause());
                return;
              }
              worlds[index] = new World(ar2.result().iterator().next().getInteger(0), randomWorld());
              if (++queryCount == worlds.length) {
                handleUpdates();
              }
            }
          });
        }
      });
    }

    void handleUpdates() {
      Arrays.sort(worlds);
      List<Tuple> batch = new ArrayList<>();
      for (World world : worlds) {
        batch.add(Tuple.of(world.getRandomNumber(), world.getId()));
      }
      UPDATE_WORLD_QUERY.executeBatch(batch, ar2 -> {
        if (ar2.failed()) {
          sendError(ar2.cause());
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
            .end(json.toBuffer(), NULL_HANDLER);
      });
    }

    void sendError(Throwable err) {
      logger.error("", err);
      req.response().setStatusCode(500).end(err.getMessage());
    }
  }

  private void handleFortunes(HttpServerRequest req) {
    SELECT_FORTUNE_QUERY.execute(ar -> {
      HttpServerResponse response = req.response();
      if (ar.succeeded()) {
        List<Fortune> fortunes = new ArrayList<>();
        RowIterator<Row> resultSet = ar.result().iterator();
        if (!resultSet.hasNext()) {
          response.setStatusCode(404).end("No results");
          return;
        }
        while (resultSet.hasNext()) {
          Row row = resultSet.next();
          fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
        }
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        response
            .putHeader(HttpHeaders.SERVER, SERVER)
            .putHeader(HttpHeaders.DATE, dateString)
            .putHeader(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_HTML)
            .end(FortunesTemplate.template(fortunes).render(factory).buffer(), NULL_HANDLER);
      } else {
        Throwable err = ar.cause();
        logger.error("", err);
        response.setStatusCode(500).end(err.getMessage());
      }
    });
  }

  private void handleCaching(HttpServerRequest req) {
    int count = 1;
    try {
      String countStr = req.getParam("count");
      if (countStr != null) {
        count = Integer.parseInt(countStr);
      }
    } catch (NumberFormatException ignore) {
    }
    count = Math.max(1, count);
    count = Math.min(500, count);
    CachedWorld[] worlds = WORLD_CACHE.getCachedWorld(count);
    JsonArray json = new JsonArray(new ArrayList<>(count));
    for (int i = 0;i < count;i++) {
      CachedWorld world = worlds[i];
      json.add(JsonObject.of("id", world.getId(), "randomNumber", world.getRandomNumber()));
    }
    HttpServerResponse response = req.response();
    MultiMap headers = response.headers();
    headers
        .add(HEADER_CONTENT_TYPE, RESPONSE_TYPE_JSON)
        .add(HEADER_SERVER, SERVER)
        .add(HEADER_DATE, dateString);
    response.end(json.toBuffer(), NULL_HANDLER);
  }

  public static void main(String[] args) throws Exception {

    int eventLoopPoolSize = VertxOptions.DEFAULT_EVENT_LOOP_POOL_SIZE;
    String sizeProp = System.getProperty("vertx.eventLoopPoolSize");
    if (sizeProp != null) {
      try {
        eventLoopPoolSize = Integer.parseInt(sizeProp);
      } catch (NumberFormatException e) {
        e.printStackTrace();
      }
    }
    JsonObject config = new JsonObject(new String(Files.readAllBytes(new File(args[0]).toPath())));
    Vertx vertx = Vertx.vertx(new VertxOptions().setEventLoopPoolSize(eventLoopPoolSize).setPreferNativeTransport(true));
    vertx.exceptionHandler(err -> {
      err.printStackTrace();
    });
    printConfig(vertx);
    vertx.deployVerticle(App.class.getName(),
        new DeploymentOptions().setInstances(eventLoopPoolSize).setConfig(config), event -> {
          if (event.succeeded()) {
            logger.info("Server listening on port " + 8080);
          } else {
            logger.error("Unable to start your application", event.cause());
          }
        });
  }

  private static void printConfig(Vertx vertx) {
    boolean nativeTransport = vertx.isNativeTransportEnabled();
    String version = "unknown";
    try {
      InputStream in = Vertx.class.getClassLoader().getResourceAsStream("META-INF/vertx/vertx-version.txt");
      if (in == null) {
        in = Vertx.class.getClassLoader().getResourceAsStream("vertx-version.txt");
      }
      ByteArrayOutputStream out = new ByteArrayOutputStream();
      byte[] buffer = new byte[256];
      while (true) {
        int amount = in.read(buffer);
        if (amount == -1) {
          break;
        }
        out.write(buffer, 0, amount);
      }
      version = out.toString();
    } catch (IOException e) {
      logger.error("Could not read Vertx version", e);;
    }
    logger.info("Vertx: " + version);
    logger.info("Event Loop Size: " + ((MultithreadEventExecutorGroup)vertx.nettyEventLoopGroup()).executorCount());
    logger.info("Native transport : " + nativeTransport);
  }
}
