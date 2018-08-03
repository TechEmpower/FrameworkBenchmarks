package io.vertx.benchmark;

import com.fasterxml.jackson.module.afterburner.AfterburnerModule;
import com.github.susom.database.Config;
import com.github.susom.database.ConfigFrom;
import com.github.susom.database.DatabaseProviderVertx;
import com.github.susom.database.DatabaseProviderVertx.Builder;
import com.github.susom.database.SqlInsert;
import com.julienviet.pgclient.*;
import io.vertx.benchmark.model.Fortune;
import io.vertx.benchmark.model.Message;
import io.vertx.benchmark.model.World;
import io.vertx.core.*;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.templ.HandlebarsTemplateEngine;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static io.vertx.benchmark.Helper.randomWorld;

public class App extends AbstractVerticle {

  static {
    Json.mapper.registerModule(new AfterburnerModule());
    Json.prettyMapper.registerModule(new AfterburnerModule());
  }

  /**
   * MongoDB implementation
   */
  private final class MongoDBBenchmark {
    private final JsonObject FIELDS = new JsonObject().put("_id", 0);

    private final MongoClient database;
    // In order to use a template we first need to create an engine
    private final HandlebarsTemplateEngine engine;

    public MongoDBBenchmark(Vertx vertx, JsonObject config) {
      final JsonObject mongoConfig = config.copy();

      // mongo is configured without credentials
      mongoConfig.remove("username");
      mongoConfig.remove("password");

      this.database = MongoClient.createShared(vertx, mongoConfig);
      this.engine = HandlebarsTemplateEngine.create();
    }

    public final void dbHandler(final RoutingContext ctx) {
      database.findOne("world", new JsonObject().put("_id", randomWorld()), FIELDS, findOne -> {
        if (findOne.failed()) {
          ctx.fail(findOne.cause());
          return;
        }

        ctx.response()
            .putHeader(HttpHeaders.SERVER, SERVER)
            .putHeader(HttpHeaders.DATE, date)
            .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
            .end(Json.encodeToBuffer(new World(findOne.result())));
      });
    }

    public final void queriesHandler(final RoutingContext ctx) {
      final int queries = Helper.getQueries(ctx.request());

      final World[] worlds = new World[queries];

      new Handler<Integer>() {
        @Override
        public void handle(Integer idx) {
          if (idx == queries) {
            // stop condition
            ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                .end(Json.encodeToBuffer(worlds));

          } else {

            final Handler<Integer> self = this;

            database.findOne("world", new JsonObject().put("_id", randomWorld()), FIELDS, findOne -> {
              if (findOne.failed()) {
                ctx.fail(findOne.cause());
                return;
              }

              worlds[idx] = new World(findOne.result());
              self.handle(idx + 1);
            });
          }
        }
      }.handle(0);
    }

    public final void fortunesHandler(final RoutingContext ctx) {
      final List<Fortune> fortunes = new LinkedList<>();

      database.find("fortune", new JsonObject(), find -> {
        if (find.failed()) {
          ctx.fail(find.cause());
          return;
        }

        for (JsonObject document : find.result()) {
          fortunes.add(new Fortune(document));
        }

        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);

        ctx.put("fortunes", fortunes);

        // and now delegate to the engine to render it.
        engine.render(ctx, "templates/fortunes.hbs", res -> {
          if (res.succeeded()) {
            ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "text/html; charset=UTF-8")
                .end(res.result());
          } else {
            ctx.fail(res.cause());
          }
        });
      });
    }

    public final void updateHandler(final RoutingContext ctx) {
      final int queries = Helper.getQueries(ctx.request());
      final World[] worlds = new World[queries];

      new Handler<Integer>() {
        @Override
        public void handle(Integer idx) {
          if (idx == queries) {
            // stop condition
            ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                .end(Json.encodeToBuffer(worlds));

          } else {

            final Handler<Integer> self = this;

            final int id = randomWorld();

            final JsonObject query = new JsonObject().put("_id", id);

            database.findOne("world", query, FIELDS, findOne -> {
              if (findOne.failed()) {
                ctx.fail(findOne.cause());
                return;
              }

              final int newRandomNumber = randomWorld();

              database.update("world", query, new JsonObject().put("$set", new JsonObject().put("randomNumber", newRandomNumber)), update -> {
                if (update.failed()) {
                  ctx.fail(update.cause());
                  return;
                }

                worlds[idx] = new World(id, newRandomNumber);
                self.handle(idx + 1);
              });
            });

          }
        }
      }.handle(0);
    }
  }

  /**
   * PgClient implementation
   */
  private final class PgClientBenchmark {

    private static final String UPDATE_WORLD = "UPDATE world SET randomnumber=$1 WHERE id=$2";
    private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    private static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";

    private final PgClient client;
    private final PgConnectionPool pool;

    // In order to use a template we first need to create an engine
    private final HandlebarsTemplateEngine engine;

    public PgClientBenchmark(Vertx vertx, JsonObject config) {
      PgClientOptions options = new PgClientOptions();
      options.setDatabase(config.getString("database"));
      options.setHost(config.getString("host"));
      options.setPort(config.getInteger("port", 5432));
      options.setUsername(config.getString("username"));
      options.setPassword(config.getString("password"));
      options.setCachePreparedStatements(true);
      client = PgClient.create(vertx, options);
      pool = client.createPool(new PgPoolOptions().setMode(PoolingMode.STATEMENT));
      this.engine = HandlebarsTemplateEngine.create();
    }

    public final void dbHandler(final RoutingContext ctx) {
      pool.getConnection(getConnection -> {
        if (getConnection.failed()) {
          ctx.fail(getConnection.cause());
          return;
        }

        final PgConnection conn = getConnection.result();
        final PgPreparedStatement worldSelect = conn.prepare(SELECT_WORLD);

        worldSelect.query(randomWorld()).execute(res -> {
          conn.close();
          if (res.succeeded()) {
            final List<JsonArray> resultSet = res.result().getResults();
            if (resultSet.isEmpty()) {
              ctx.response()
                .setStatusCode(404)
                .end();
              return;
            }
            final JsonArray row = resultSet.get(0);
            ctx.response()
              .putHeader(HttpHeaders.SERVER, SERVER)
              .putHeader(HttpHeaders.DATE, date)
              .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
              .end(Json.encodeToBuffer(new World(row.getInteger(0), row.getInteger(1))));
          } else {
            ctx.fail(res.cause());
          }
        });
      });
    }

    public final void queriesHandler(final RoutingContext ctx) {

      pool.getConnection(getConnection -> {
        if (getConnection.failed()) {
          ctx.fail(getConnection.cause());
          return;
        }

        final PgConnection conn = getConnection.result();
        final PgPreparedStatement worldSelect = conn.prepare(SELECT_WORLD);
        final int queries = Helper.getQueries(ctx.request());
        final World[] worlds = new World[queries];
        final boolean[] failed = { false };
        final int[] cnt = { 0 };

        for (int i = 0; i < queries; i++) {
          worldSelect.query(randomWorld()).execute(ar -> {
            if (!failed[0]) {
              if (ar.failed()) {
                conn.close();
                failed[0] = true;
                ctx.fail(ar.cause());
                return;
              }

              // we need a final reference
              final JsonArray row = ar.result().getResults().get(0);
              worlds[cnt[0]++] = new World(row.getInteger(0), row.getInteger(1));

              // stop condition
              if (cnt[0] == queries) {
                conn.close();
                ctx.response()
                  .putHeader(HttpHeaders.SERVER, SERVER)
                  .putHeader(HttpHeaders.DATE, date)
                  .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                  .end(Json.encodeToBuffer(worlds));
              }
            }
          });
        }
      });
    }

    public final void fortunesHandler(final RoutingContext ctx) {

      pool.getConnection(getConnection -> {
        if (getConnection.failed()) {
          ctx.fail(getConnection.cause());
          return;
        }

        final PgConnection conn = getConnection.result();
        final PgPreparedStatement fortuneSelect = conn.prepare(SELECT_FORTUNE);
        fortuneSelect.query().execute(ar -> {
          conn.close();
          if (ar.succeeded()) {
            final List<JsonArray> resultSet = ar.result().getResults();
            if (resultSet == null || resultSet.size() == 0) {
              ctx.fail(404);
              return;
            }

            final List<Fortune> fortunes = new ArrayList<>(resultSet.size() + 1);

            for (JsonArray row : resultSet) {
              fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
            }

            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            Collections.sort(fortunes);

            ctx.put("fortunes", fortunes);

            // and now delegate to the engine to render it.
            engine.render(ctx, "templates", "/fortunes.hbs", res -> {
              if (res.succeeded()) {
                ctx.response()
                        .putHeader(HttpHeaders.SERVER, SERVER)
                        .putHeader(HttpHeaders.DATE, date)
                        .putHeader(HttpHeaders.CONTENT_TYPE, "text/html; charset=UTF-8")
                        .end(res.result());
              } else {
                ctx.fail(res.cause());
              }
            });
          } else {
            ctx.fail(ar.cause());
          }
        });
      });
    }

    public final void updateHandler(final RoutingContext ctx) {

      pool.getConnection(getConnection -> {
        if (getConnection.failed()) {
          ctx.fail(getConnection.cause());
          return;
        }

        final PgConnection conn = getConnection.result();

        final PgPreparedStatement worldSelect = conn.prepare(SELECT_WORLD);
        final int queries = Helper.getQueries(ctx.request());
        final World[] worlds = new World[queries];
        final boolean[] failed = { false };
        final int[] queryCount = { 0 };

        for (int i = 0; i < worlds.length; i++) {
          int id = randomWorld();
          worldSelect.query(id).execute(ar2 -> {
            if (!failed[0]) {
              if (ar2.failed()) {
                failed[0] = true;
                conn.close();
                ctx.fail(ar2.cause());
                return;
              }

              final JsonArray row = ar2.result().getResults().get(0);
              worlds[queryCount[0]++] = new World(row.getInteger(0), randomWorld());

              if (queryCount[0] == worlds.length) {
                Arrays.sort(worlds);
                PgPreparedStatement worldUpdate = conn.prepare(UPDATE_WORLD);
                PgBatch batch = worldUpdate.batch();

                for (World world : worlds) {
                  batch.add(world.getRandomNumber(), world.getId());
                }
                batch.execute(ar3 -> {
                  conn.close();
                  if (ar3.failed()) {
                    ctx.fail(ar3.cause());
                    return;
                  }

                  ctx.response()
                    .putHeader(HttpHeaders.SERVER, SERVER)
                    .putHeader(HttpHeaders.DATE, date)
                    .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                    .end(Json.encodeToBuffer(worlds));
                });
              }
            }
          });
        }
      });
    }
  }

  /**
   * Implementation using com.github.susom:database library and standard JDBC driver.
   */
  private final class DatabaseSqlBenchmark {
    private final Builder dbBuilder;

    // In order to use a template we first need to create an engine
    private final HandlebarsTemplateEngine engine;

    DatabaseSqlBenchmark(Vertx vertx, JsonObject jsonConfig) {
      Config config = ConfigFrom.firstOf().custom(jsonConfig::getString)
          .rename("username", "database.user")
          .rename("password", "database.password")
          .rename("maxPoolSize", "database.pool.size")
          .value("database.url", "jdbc:postgresql://" + jsonConfig.getString("host") + "/" + jsonConfig.getString("database"))
          .get();
      dbBuilder = DatabaseProviderVertx.pooledBuilder(vertx, config);
      engine = HandlebarsTemplateEngine.create();
    }

    void dbHandler(final RoutingContext ctx) {
      dbBuilder.transactAsync(dbs ->
        dbs.get().toSelect("SELECT id, randomnumber from WORLD where id = ?")
            .argInteger(randomWorld())
            .queryFirstOrNull(row -> new World(row.getIntegerOrZero(), row.getIntegerOrZero()))
      , call -> {
        if (call.succeeded()) {
          if (call.result() == null) {
            ctx.fail(404);
          } else {
            ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                .end(Json.encode(call.result()));
          }
        } else {
          ctx.fail(call.cause());
        }
      });
    }

    void queriesHandler(final RoutingContext ctx) {
      int queries = Helper.getQueries(ctx.request());

      dbBuilder.transactAsync(dbs -> {
        List<World> worlds = new ArrayList<>();
        for (int i = 1; i <= queries; i++) {
          dbs.get()
              .toSelect("SELECT id, randomnumber from WORLD where id = ?")
              .argInteger(randomWorld())
              .queryFirstOrNull(row -> worlds.add(new World(row.getIntegerOrZero(), row.getIntegerOrZero())));
        }
        return worlds;
      }, call -> {
        if (call.succeeded()) {
          if (call.result() == null) {
            ctx.fail(404);
          } else {
            ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                .end(Json.encodeToBuffer(call.result()));
          }
        } else {
          ctx.fail(call.cause());
        }
      });
    }

    final void fortunesHandler(final RoutingContext ctx) {
      dbBuilder.transactAsync(dbs -> {
        List<Fortune> fortunes = dbs.get()
            .toSelect("SELECT id, message from FORTUNE")
            .queryMany(row -> new Fortune(row.getIntegerOrZero(), row.getStringOrEmpty()));
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        return fortunes;
      }, call -> {
        if (call.succeeded()) {
          if (call.result() == null || call.result().isEmpty()) {
            ctx.fail(404);
          } else {
            ctx.put("fortunes", call.result());
            engine.render(ctx, "templates/fortunes.hbs", res -> {
              if (res.succeeded()) {
                ctx.response()
                    .putHeader(HttpHeaders.SERVER, SERVER)
                    .putHeader(HttpHeaders.DATE, date)
                    .putHeader(HttpHeaders.CONTENT_TYPE, "text/html; charset=UTF-8")
                    .end(res.result());
              } else {
                ctx.fail(res.cause());
              }
            });
          }
        } else {
          ctx.fail(call.cause());
        }
      });
    }

    void updateHandler(final RoutingContext ctx) {
      int queries = Helper.getQueries(ctx.request());

      dbBuilder.transactAsync(dbs -> {
        List<World> worlds = new ArrayList<>();
        // Haven't implemented batching yet on toUpdate(), so hijack toInsert() as work-around
        SqlInsert batchUpdate = dbs.get().toInsert("UPDATE WORLD SET randomnumber = ? WHERE id = ?");
        for (int i = 1; i <= queries; i++) {
          World oldWorld = dbs.get()
              .toSelect("SELECT id, randomnumber from WORLD where id = ?")
              .argInteger(randomWorld())
              .queryFirstOrNull(row -> new World(row.getIntegerOrZero(), row.getIntegerOrZero()));
          if (oldWorld == null) {
            return null;
          }
          World newWorld = new World(oldWorld.getId(), randomWorld());
          worlds.add(newWorld);
          batchUpdate.argInteger(newWorld.getRandomNumber()).argInteger(newWorld.getId()).batch();
        }
        batchUpdate.insertBatchUnchecked();
        return worlds;
      }, call -> {
        if (call.succeeded()) {
          if (call.result() == null) {
            ctx.fail(404);
          } else {
            ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                .end(Json.encodeToBuffer(call.result()));
          }
        } else {
          ctx.fail(call.cause());
        }
      });
    }
  }

  private static final String SERVER = "vertx-web";
  private String date;

  @Override
  public void start() {
    final Router app = Router.router(vertx);

    vertx.setPeriodic(1000, handler -> date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));

    final MongoDBBenchmark mongoDBBenchmark = new MongoDBBenchmark(vertx, config());
    final PgClientBenchmark pgClientBenchmark= new PgClientBenchmark(vertx, config());
    final DatabaseSqlBenchmark databaseSqlBenchmark = new DatabaseSqlBenchmark(vertx, config());

    /*
     * This test exercises the framework fundamentals including keep-alive support, request routing, request header
     * parsing, object instantiation, JSON serialization, response header generation, and request count throughput.
     */
    app.get("/json").handler(ctx -> {
      ctx.response()
          .putHeader(HttpHeaders.SERVER, SERVER)
          .putHeader(HttpHeaders.DATE, date)
          .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
          .end(Json.encodeToBuffer(new Message("Hello, World!")));
    });

    /*
     * This test exercises the framework's object-relational mapper (ORM), random number generator, database driver,
     * and database connection pool.
     */
    app.get("/mongo/db").handler(mongoDBBenchmark::dbHandler);
    app.get("/psql/db").handler(pgClientBenchmark::dbHandler);
    app.get("/dbpsql/db").handler(databaseSqlBenchmark::dbHandler);

    /*
     * This test is a variation of Test #2 and also uses the World table. Multiple rows are fetched to more dramatically
     * punish the database driver and connection pool. At the highest queries-per-request tested (20), this test
     * demonstrates all frameworks' convergence toward zero requests-per-second as database activity increases.
     */
    app.get("/mongo/queries").handler(mongoDBBenchmark::queriesHandler);
    app.get("/psql/queries").handler(pgClientBenchmark::queriesHandler);
    app.get("/dbpsql/queries").handler(databaseSqlBenchmark::queriesHandler);

    /*
     * This test exercises the ORM, database connectivity, dynamic-size collections, sorting, server-side templates,
     * XSS countermeasures, and character encoding.
     */
    app.get("/mongo/fortunes").handler(mongoDBBenchmark::fortunesHandler);
    app.get("/psql/fortunes").handler(pgClientBenchmark::fortunesHandler);
    app.get("/dbpsql/fortunes").handler(databaseSqlBenchmark::fortunesHandler);

    /*
     * This test is a variation of Test #3 that exercises the ORM's persistence of objects and the database driver's
     * performance at running UPDATE statements or similar. The spirit of this test is to exercise a variable number of
     * read-then-write style database operations.
     */
    app.route("/mongo/update").handler(mongoDBBenchmark::updateHandler);
    app.route("/psql/update").handler(pgClientBenchmark::updateHandler);
    app.route("/dbpsql/update").handler(databaseSqlBenchmark::updateHandler);

    /*
     * This test is an exercise of the request-routing fundamentals only, designed to demonstrate the capacity of
     * high-performance platforms in particular. Requests will be sent using HTTP pipelining. The response payload is
     * still small, meaning good performance is still necessary in order to saturate the gigabit Ethernet of the test
     * environment.
     */
    app.get("/plaintext").handler(ctx -> {
      ctx.response()
          .putHeader(HttpHeaders.SERVER, SERVER)
          .putHeader(HttpHeaders.DATE, date)
          .putHeader(HttpHeaders.CONTENT_TYPE, "text/plain")
          .end("Hello, World!");
    });

    vertx.createHttpServer().requestHandler(app::accept).listen(8080);
  }
}
