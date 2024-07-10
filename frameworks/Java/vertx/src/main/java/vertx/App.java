package vertx;

import com.fizzed.rocker.ContentType;
import com.fizzed.rocker.RockerOutputFactory;
import io.netty.util.concurrent.MultithreadEventExecutorGroup;
import io.vertx.core.impl.VertxInternal;
import io.vertx.pgclient.*;
import io.vertx.core.*;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.core.http.HttpServerRequest;
import io.vertx.core.http.HttpServerResponse;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.sqlclient.*;
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
  private SqlClientInternal client1;
  private SqlClientInternal client2;
  private CharSequence dateString;
  private CharSequence[] plaintextHeaders;

  private final RockerOutputFactory<BufferRockerOutput> factory = BufferRockerOutput.factory(ContentType.RAW);

  private Throwable databaseErr;
  private PreparedQuery<RowSet<Row>> SELECT_WORLD_QUERY;
  private PreparedQuery<SqlResult<List<Fortune>>> SELECT_FORTUNE_QUERY;
  private PreparedQuery<RowSet<Row>> UPDATE_WORLD_QUERY;
  @SuppressWarnings("unchecked")
  private PreparedQuery<RowSet<Row>>[] AGGREGATED_UPDATE_WORLD_QUERY = new PreparedQuery[128];
  private WorldCache WORLD_CACHE;

  public static CharSequence createDateHeader() {
    return HttpHeaders.createOptimized(DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
  }

  @Override
  public void start(Promise<Void> startPromise) throws Exception {
    int port = 8080;
    server = vertx.createHttpServer(new HttpServerOptions())
            .requestHandler(App.this);
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
    Future<?> clientsInit = initClients(options);
    clientsInit
            .transform(ar -> {
              databaseErr = ar.cause();
              return server.listen(port);
            })
            .<Void>mapEmpty()
            .onComplete(startPromise);
  }

  private Future<?> initClients(PgConnectOptions options) {
    Future<?> cf1 = PgConnection.connect(vertx, options)
            .flatMap(conn -> {
              client1 = (SqlClientInternal) conn;
              List<Future<?>> list = new ArrayList<>();
              Future<PreparedStatement> f1 = conn.prepare(SELECT_WORLD)
                      .andThen(onSuccess(ps -> SELECT_WORLD_QUERY = ps.query()));
              list.add(f1);
              Future<PreparedStatement> f2 = conn.prepare(SELECT_FORTUNE)
                      .andThen(onSuccess(ps -> {
                        SELECT_FORTUNE_QUERY = ps.query().
                                collecting(Collectors.mapping(row -> new Fortune(row.getInteger(0), row.getString(1)), Collectors.toList()));
                      }));
              list.add(f2);
              Future<WorldCache> f3 = conn.preparedQuery(SELECT_WORLDS)
                      .collecting(Collectors.mapping(row -> new CachedWorld(row.getInteger(0), row.getInteger(1)), Collectors.toList()))
                      .execute()
                      .map(worlds -> new WorldCache(worlds.value()))
                      .andThen(onSuccess(wc -> WORLD_CACHE = wc));
              list.add(f3);
              return Future.join(list);
            });
    Future<?> cf2 = PgConnection.connect(vertx, options)
            .flatMap(conn -> {
              client2 = (SqlClientInternal) conn;
              List<Future<?>> list = new ArrayList<>();
              Future<PreparedStatement> f1 = conn.prepare(UPDATE_WORLD)
                      .andThen(onSuccess(ps -> UPDATE_WORLD_QUERY = ps.query()));
              list.add(f1);
              for (int i = 0; i < AGGREGATED_UPDATE_WORLD_QUERY.length; i++) {
                int idx = i;
                Future<PreparedStatement> fut = conn
                        .prepare(buildAggregatedUpdateQuery(1 + idx))
                        .andThen(onSuccess(ps -> AGGREGATED_UPDATE_WORLD_QUERY[idx] = ps.query()));
                list.add(fut);
              }
              return Future.join(list);
            });
    return Future.join(cf1, cf2);
  }

  private static String buildAggregatedUpdateQuery(int len) {
    StringBuilder sb = new StringBuilder();
    sb.append("UPDATE world SET randomNumber = update_data.randomNumber FROM (VALUES");
    char sep = ' ';
    for (int i = 1;i <= len;i++) {
      sb.append(sep).append("($").append(2 * i - 1).append("::int,$").append(2 * i).append("::int)");
      sep = ',';
    }
    sb.append(") AS update_data (id, randomNumber) WHERE  world.id = update_data.id");
    return sb.toString();
  }

  private static <T> Handler<AsyncResult<T>> onSuccess(Handler<T> handler) {
    return ar -> {
      if (ar.succeeded()) {
        handler.handle(ar.result());
      }
    };
  }

  @Override
  public void handle(HttpServerRequest request) {
    try {
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
    } catch (Exception e) {
      sendError(request, e);
    }
  }

  @Override
  public void stop() {
    if (server != null) server.close();
  }

  private void sendError(HttpServerRequest req, Throwable cause) {
    logger.error(cause.getMessage(), cause);
    req.response().setStatusCode(500).end();
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
            .end(new World(row.getInteger(0), row.getInteger(1)).toBuffer(), NULL_HANDLER);
      } else {
        sendError(req, res.cause());
      }
    });
  }

  class Queries implements Handler<AsyncResult<RowSet<Row>>> {

    boolean failed;
    final JsonArray worlds;
    final HttpServerRequest req;
    final HttpServerResponse resp;
    final int queries;

    public Queries(HttpServerRequest req) {
      int queries = getQueries(req);

      this.req = req;
      this.resp = req.response();
      this.queries = queries;
      this.worlds = new JsonArray(new ArrayList<>(queries));
    }

    private void handle() {
      client1.group(c -> {
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
          sendError(req, ar.cause());
          return;
        }

        // we need a final reference
        final Tuple row = ar.result().iterator().next();
        worlds.add(new JsonObject().put("id", row.getInteger(0)).put("randomNumber", row.getInteger(1)));

        // stop condition
        if (worlds.size() == queries) {
          resp
              .putHeader(HttpHeaders.SERVER, SERVER)
              .putHeader(HttpHeaders.DATE, dateString)
              .putHeader(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_JSON)
              .end(worlds.toBuffer(), NULL_HANDLER);
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

      client1.group(c -> {
        PreparedQuery<RowSet<Row>> preparedQuery = c.preparedQuery(SELECT_WORLD);
        for (int i = 0; i < worlds.length; i++) {
          int id = randomWorld();
          int index = i;
          preparedQuery.execute(Tuple.of(id), ar2 -> {
            if (!failed) {
              if (ar2.failed()) {
                failed = true;
                sendError(req, ar2.cause());
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
      int len = worlds.length;
      if (0 < len && len <= AGGREGATED_UPDATE_WORLD_QUERY.length) {
        List<Object> arguments = new ArrayList<>();
        for (World world : worlds) {
            arguments.add(world.getId());
            arguments.add(world.getRandomNumber());
        }
        Tuple tuple = Tuple.tuple(arguments);
        PreparedQuery<RowSet<Row>> query = AGGREGATED_UPDATE_WORLD_QUERY[len - 1];
        query.execute(tuple, this::sendResponse);
      } else {
        List<Tuple> batch = new ArrayList<>();
        for (World world : worlds) {
          batch.add(Tuple.of(world.getRandomNumber(), world.getId()));
        }
        UPDATE_WORLD_QUERY.executeBatch(batch, this::sendResponse);
      }
    }

    private void sendResponse(AsyncResult<?> res) {
      if (res.failed()) {
        sendError(req, res.cause());
        return;
      }
      JsonArray json = new JsonArray();
      for (World world : worlds) {
        json.add(world);
      }
      req.response()
              .putHeader(HttpHeaders.SERVER, SERVER)
              .putHeader(HttpHeaders.DATE, dateString)
              .putHeader(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_JSON)
              .end(json.toBuffer(), NULL_HANDLER);
    }

  }

  private void handleFortunes(HttpServerRequest req) {
    SELECT_FORTUNE_QUERY.execute(ar -> {
      HttpServerResponse response = req.response();
      if (ar.succeeded()) {
        SqlResult<List<Fortune>> result = ar.result();
        if (result.size() == 0) {
          response.setStatusCode(404).end("No results");
          return;
        }
        List<Fortune> fortunes = result.value();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        response
            .putHeader(HttpHeaders.SERVER, SERVER)
            .putHeader(HttpHeaders.DATE, dateString)
            .putHeader(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_HTML)
            .end(FortunesTemplate.template(fortunes).render(factory).buffer(), NULL_HANDLER);
      } else {
        sendError(req, ar.cause());
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
    List<CachedWorld> worlds = WORLD_CACHE.getCachedWorld(count);
    JsonArray json = new JsonArray(worlds);
    HttpServerResponse response = req.response();
    MultiMap headers = response.headers();
    headers
        .add(HEADER_CONTENT_TYPE, RESPONSE_TYPE_JSON)
        .add(HEADER_SERVER, SERVER)
        .add(HEADER_DATE, dateString);
    response.end(json.toBuffer(), NULL_HANDLER);
  }

  public static void main(String[] args) throws Exception {

    int eventLoopPoolSize = Runtime.getRuntime().availableProcessors();
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
    String transport = ((VertxInternal) vertx).transport().getClass().getSimpleName();
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
    logger.info("Transport : " + transport);
  }
}
