package io.vertx.benchmark;

import com.fasterxml.jackson.module.blackbird.BlackbirdModule;
import io.vertx.benchmark.model.Fortune;
import io.vertx.benchmark.model.Message;
import io.vertx.benchmark.model.World;
import io.vertx.core.*;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import io.vertx.core.json.jackson.DatabindCodec;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.templ.rocker.RockerTemplateEngine;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgConnection;
import io.vertx.sqlclient.*;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static io.vertx.benchmark.Helper.randomWorld;

public class App extends AbstractVerticle {

  static {
    DatabindCodec.mapper().registerModule(new BlackbirdModule());
    DatabindCodec.prettyMapper().registerModule(new BlackbirdModule());
  }

  // TODO: this function can be moved into `PgClientBenchmark`, made static, and renamed when static declarations in inner classes are supported (when the JDK is upgraded to 16 or above).
  private void createPgClientBenchmarkAsync(Vertx vertx, JsonObject config, Handler<PgClientBenchmark> onComplete) {
    PgConnectOptions options = new PgConnectOptions()
        .setCachePreparedStatements(true)
        .setHost(config.getString("host"))
        .setPort(config.getInteger("port", 5432))
        .setUser(config.getString("username"))
        .setPassword(config.getString("password"))
        .setDatabase(config.getString("database"))
        .setPipeliningLimit(100_000); // Large pipelining means less flushing and we use a single connection anyway;

    PgConnection.connect(vertx, options).onComplete(ar ->
      onComplete.handle(new PgClientBenchmark(ar.result(), RockerTemplateEngine.create()))
    );
  }

  /**
   * PgClient implementation
   */
  private final class PgClientBenchmark {

    private static final String UPDATE_WORLD = "UPDATE world SET randomnumber=$1 WHERE id=$2";
    private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    private static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";

    private final PgConnection client;

    // In order to use a template we first need to create an engine
    private final RockerTemplateEngine engine;

    private PgClientBenchmark(PgConnection client, RockerTemplateEngine engine) {
      this.client = client;
      this.engine = engine;
    }

    public void dbHandler(final RoutingContext ctx) {
      client
        .preparedQuery(SELECT_WORLD)
        .execute(Tuple.of(randomWorld()), res -> {
        if (res.succeeded()) {
          final RowIterator<Row> resultSet = res.result().iterator();
          if (!resultSet.hasNext()) {
            ctx.response()
              .setStatusCode(404)
              .end();
            return;
          }
          final Row row = resultSet.next();
          ctx.response()
            .putHeader(HttpHeaders.SERVER, SERVER)
            .putHeader(HttpHeaders.DATE, date)
            .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
            .end(Json.encodeToBuffer(new World(row.getInteger(0), row.getInteger(1))));
        } else {
          res.cause().printStackTrace();
          ctx.fail(res.cause());
        }
      });
    }

    public void queriesHandler(final RoutingContext ctx) {

      final int queries = Helper.getQueries(ctx.request());
      final World[] worlds = new World[queries];
      final boolean[] failed = { false };
      final int[] cnt = { 0 };

      for (int i = 0; i < queries; i++) {
        client.preparedQuery(SELECT_WORLD).execute(Tuple.of(randomWorld()), res -> {
          if (!failed[0]) {
            if (res.failed()) {
              failed[0] = true;
              ctx.fail(res.cause());
              return;
            }

            // we need a final reference
            final Row row = res.result().iterator().next();
            worlds[cnt[0]++] = new World(row.getInteger(0), row.getInteger(1));

            // stop condition
            if (cnt[0] == queries) {
              ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                .end(Json.encodeToBuffer(worlds));
            }
          }
        });
      }
    }

    public void fortunesHandler(final RoutingContext ctx) {

      client.preparedQuery(SELECT_FORTUNE).execute(ar -> {
          if (ar.succeeded()) {
            final RowIterator<Row> resultSet = ar.result().iterator();
            if (!resultSet.hasNext()) {
              ctx.fail(404);
              return;
            }

            final List<Fortune> fortunes = new ArrayList<>();

            while (resultSet.hasNext()) {
                final Row row = resultSet.next();
              fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
            }

            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            Collections.sort(fortunes);

            ctx.put("fortunes", fortunes);

            // and now delegate to the engine to render it.
            engine.render(ctx.data(), "templates/Fortunes.rocker.html", res -> {
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
    }

    public void updateHandler(final RoutingContext ctx) {

         final int queries = Helper.getQueries(ctx.request());
         final World[] worlds = new World[queries];
         final boolean[] failed = { false };
         final int[] queryCount = { 0 };

         for (int i = 0; i < worlds.length; i++) {
           int id = randomWorld();
           client.preparedQuery(SELECT_WORLD).execute(Tuple.of(id), ar2 -> {
             if (!failed[0]) {
               if (ar2.failed()) {
                 failed[0] = true;
                 ctx.fail(ar2.cause());
                 return;
               }

               final Row row = ar2.result().iterator().next();
               worlds[queryCount[0]++] = new World(row.getInteger(0), randomWorld());

               if (queryCount[0] == worlds.length) {
                 Arrays.sort(worlds);
                 List<Tuple> batch = new ArrayList<>();

                 for (World world : worlds) {
                   batch.add(Tuple.of(world.getRandomNumber(), world.getId()));
                 }

                 client.preparedQuery(UPDATE_WORLD).executeBatch(batch, ar3 -> {
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
    }
  }

  private static final String SERVER = "vertx-web";
  private String date;

  @Override
  public void start(Promise<Void> startPromise) {
    final Router app = Router.router(vertx);
    // initialize the date header
    date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now());
    // refresh the value as a periodic task
    vertx.setPeriodic(1000, handler -> date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));

    createPgClientBenchmarkAsync(vertx, config(), pgClientBenchmark -> {
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
      app.get("/db").handler(pgClientBenchmark::dbHandler);

      /*
       * This test is a variation of Test #2 and also uses the World table. Multiple rows are fetched to more dramatically
       * punish the database driver and connection pool. At the highest queries-per-request tested (20), this test
       * demonstrates all frameworks' convergence toward zero requests-per-second as database activity increases.
       */
      app.get("/queries").handler(pgClientBenchmark::queriesHandler);

      /*
       * This test exercises the ORM, database connectivity, dynamic-size collections, sorting, server-side templates,
       * XSS countermeasures, and character encoding.
       */
      app.get("/fortunes").handler(pgClientBenchmark::fortunesHandler);

      /*
       * This test is a variation of Test #3 that exercises the ORM's persistence of objects and the database driver's
       * performance at running UPDATE statements or similar. The spirit of this test is to exercise a variable number of
       * read-then-write style database operations.
       */
      app.route("/update").handler(pgClientBenchmark::updateHandler);

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

      vertx.createHttpServer().requestHandler(app).listen(8080, listen -> {
        if (listen.failed()) {
          listen.cause().printStackTrace();
          System.exit(1);
        }
        startPromise.complete();
      });
    });
  }
}
