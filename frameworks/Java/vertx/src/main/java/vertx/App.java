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
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.sqlclient.*;
import io.vertx.sqlclient.impl.SqlClientInternal;
import vertx.model.*;
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
import java.util.stream.IntStream;

public class App extends AbstractVerticle implements Handler<HttpServerRequest> {

  private static final int NUM_PROCESSORS = Runtime.getRuntime().availableProcessors();
  private static final org.slf4j.Logger log = org.slf4j.LoggerFactory.getLogger(App.class);

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

  private static Logger logger = LoggerFactory.getLogger(App.class.getName());

  private static final Integer[] BOXED_RND = IntStream.range(1, 10001).boxed().toArray(Integer[]::new);

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
  static final CharSequence RESPONSE_TYPE_JSON = HttpHeaders.createOptimized("application/json");

  private static final String HELLO_WORLD = "Hello, world!";
  private static final Buffer HELLO_WORLD_BUFFER = Buffer.buffer(HELLO_WORLD, "UTF-8");

  private static final CharSequence HEADER_SERVER = HttpHeaders.createOptimized("server");
  private static final CharSequence HEADER_DATE = HttpHeaders.createOptimized("date");
  private static final CharSequence HEADER_CONTENT_TYPE = HttpHeaders.createOptimized("content-type");
  private static final CharSequence HEADER_CONTENT_LENGTH = HttpHeaders.createOptimized("content-length");

  private static final CharSequence HELLO_WORLD_LENGTH = HttpHeaders.createOptimized("" + HELLO_WORLD.length());
  private static final CharSequence SERVER = HttpHeaders.createOptimized("vert.x");

  private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
  private static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";
  private static final String SELECT_WORLDS = "SELECT id, randomnumber from WORLD";

  public static CharSequence createDateHeader() {
    return HttpHeaders.createOptimized(DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
  }

  /**
   * Returns a random integer that is a suitable value for both the {@code id}
   * and {@code randomNumber} properties of a world object.
   *
   * @return a random world number
   */
  static Integer boxedRandomWorldNumber() {
    final int rndValue = ThreadLocalRandom.current().nextInt(1, 10001);
    final var boxedRnd = BOXED_RND[rndValue - 1];
    assert boxedRnd.intValue() == rndValue;
    return boxedRnd;
  }

  private HttpServer server;
  private SqlClientInternal client;
  private CharSequence dateString;
  private CharSequence[] plaintextHeaders;

  private final RockerOutputFactory<BufferRockerOutput> factory = BufferRockerOutput.factory(ContentType.RAW);

  private Throwable databaseErr;
  private PreparedQuery<RowSet<Row>> SELECT_WORLD_QUERY;
  private PreparedQuery<SqlResult<List<Fortune>>> SELECT_FORTUNE_QUERY;
  @SuppressWarnings("unchecked")
  private PreparedQuery<RowSet<Row>>[] AGGREGATED_UPDATE_WORLD_QUERY = new PreparedQuery[500];
  private WorldCache WORLD_CACHE;

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
    options.setPreparedStatementCacheMaxSize(1024);
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
    return PgConnection.connect(vertx, options)
            .flatMap(conn -> {
              client = (SqlClientInternal) conn;
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
              for (int i = 0; i < AGGREGATED_UPDATE_WORLD_QUERY.length; i++) {
                int idx = i;
                Future<PreparedStatement> fut = conn
                        .prepare(buildAggregatedUpdateQuery(1 + idx))
                        .andThen(onSuccess(ps -> AGGREGATED_UPDATE_WORLD_QUERY[idx] = ps.query()));
                list.add(fut);
              }
              return Future.join(list);
            });
  }

  private static String buildAggregatedUpdateQuery(int len) {
    StringBuilder sql = new StringBuilder();
    sql.append("UPDATE WORLD SET RANDOMNUMBER = CASE ID");
    for (int i = 0; i < len; i++) {
      int offset = (i * 2) + 1;
      sql.append(" WHEN $").append(offset).append(" THEN $").append(offset + 1);
    }
    sql.append(" ELSE RANDOMNUMBER");
    sql.append(" END WHERE ID IN ($1");
    for (int i = 1; i < len; i++) {
      int offset = (i * 2) + 1;
      sql.append(",$").append(offset);
    }
    sql.append(")");
    return sql.toString();
  }

  public static <T> Handler<AsyncResult<T>> onSuccess(Handler<T> handler) {
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
          request.response()
                  .setStatusCode(404)
                  .end();
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
    response.end(new Message("Hello, World!").toJson(), NULL_HANDLER);
  }

  private void handleDb(HttpServerRequest req) {
    HttpServerResponse resp = req.response();
    SELECT_WORLD_QUERY.execute(Tuple.of(boxedRandomWorldNumber()), res -> {
      if (res.succeeded()) {
        RowIterator<Row> resultSet = res.result().iterator();
        if (!resultSet.hasNext()) {
          resp.setStatusCode(404).end();
          return;
        }
        Row row = resultSet.next();
        World word = new World(row.getInteger(0), row.getInteger(1));
        MultiMap headers = resp.headers();
        headers.add(HttpHeaders.SERVER, SERVER);
        headers.add(HttpHeaders.DATE, dateString);
        headers.add(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_JSON);
        resp.end(word.toJson(), NULL_HANDLER);
      } else {
        sendError(req, res.cause());
      }
    });
  }

  class Queries implements Handler<AsyncResult<RowSet<Row>>> {

    boolean failed;
    final World[] worlds;
    final HttpServerRequest req;
    final HttpServerResponse resp;
    final int queries;
    int worldsIndex;

    public Queries(HttpServerRequest req) {
      int queries = getQueries(req);

      this.req = req;
      this.resp = req.response();
      this.queries = queries;
      this.worlds = new World[queries];
      this.worldsIndex = 0;
    }

    private void handle() {
      client.group(c -> {
        for (int i = 0; i < queries; i++) {
          c.preparedQuery(SELECT_WORLD).execute(Tuple.of(boxedRandomWorldNumber()), this);
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
        worlds[worldsIndex++] = new World(row.getInteger(0), row.getInteger(1));

        // stop condition
        if (worldsIndex == queries) {
          MultiMap headers = resp.headers();
          headers.add(HttpHeaders.SERVER, SERVER);
          headers.add(HttpHeaders.DATE, dateString);
          headers.add(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_JSON);
          resp.end(World.toJson(worlds), NULL_HANDLER);
        }
      }
    }
  }

  private class Update {

    private final HttpServerRequest request;
    private final World[] worldsToUpdate;
    private boolean failed;
    private int selectWorldCompletedCount;

    public Update(HttpServerRequest request) {
      this.request = request;
      this.worldsToUpdate = new World[getQueries(request)];
    }

    public void handle() {
      client.group(c -> {
        final PreparedQuery<RowSet<Row>> preparedQuery = c.preparedQuery(App.SELECT_WORLD);
        for (int i = 0; i < worldsToUpdate.length; i++) {
          final Integer id = boxedRandomWorldNumber();
          final int index = i;
          preparedQuery.execute(Tuple.of(id), res -> {
            if (!failed) {
              if (res.failed()) {
                failed = true;
                sendError(request, res.cause());
                return;
              }
              worldsToUpdate[index] = new World(res.result().iterator().next().getInteger(0), boxedRandomWorldNumber());
              if (++selectWorldCompletedCount == worldsToUpdate.length) {
                randomWorldsQueryCompleted();
              }
            }
          });
        }
      });
    }

    private void randomWorldsQueryCompleted() {
      Arrays.sort(worldsToUpdate);
      final List<Integer> params = new ArrayList<>(worldsToUpdate.length * 2);
      for (int i = 0, count = worldsToUpdate.length;i < count;i++) {
        var world = worldsToUpdate[i];
        params.add(world.getId());
        params.add(world.getRandomNumber());
      }
      AGGREGATED_UPDATE_WORLD_QUERY[worldsToUpdate.length - 1].execute(Tuple.wrap(params), updateResult -> {
        if (updateResult.failed()) {
          sendError(request, updateResult.cause());
          return;
        }
        sendResponse();
      });
    }

    private void sendResponse() {
      var res = request.response();
      MultiMap headers = res.headers();
      headers.add(HttpHeaders.SERVER, App.SERVER);
      headers.add(HttpHeaders.DATE, dateString);
      headers.add(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_JSON);
      Buffer buff = WorldJsonSerializer.toJsonBuffer(worldsToUpdate);
      res.end(buff, null);
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
        MultiMap headers = response.headers();
        headers.add(HttpHeaders.SERVER, SERVER);
        headers.add(HttpHeaders.DATE, dateString);
        headers.add(HttpHeaders.CONTENT_TYPE, RESPONSE_TYPE_HTML);
        FortunesTemplate template = FortunesTemplate.template(fortunes);
        response.end(template.render(factory).buffer(), NULL_HANDLER);
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
    HttpServerResponse response = req.response();
    MultiMap headers = response.headers();
    headers
        .add(HEADER_CONTENT_TYPE, RESPONSE_TYPE_JSON)
        .add(HEADER_SERVER, SERVER)
        .add(HEADER_DATE, dateString);
    response.end(CachedWorld.toJson(worlds), NULL_HANDLER);
  }

  public static void main(String[] args) throws Exception {
    int eventLoopPoolSize = NUM_PROCESSORS;
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
    logger.info("Processors: " + NUM_PROCESSORS);
    logger.info("Event Loop Size: " + ((MultithreadEventExecutorGroup)vertx.nettyEventLoopGroup()).executorCount());
    logger.info("Native transport : " + nativeTransport);
    logger.info("Transport : " + transport);
  }
}
