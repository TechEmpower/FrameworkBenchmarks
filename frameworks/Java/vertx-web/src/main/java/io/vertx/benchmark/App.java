package io.vertx.benchmark;

import io.vertx.benchmark.model.Fortune;
import io.vertx.benchmark.model.Message;
import io.vertx.benchmark.model.World;
import io.vertx.core.*;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.asyncsql.AsyncSQLClient;
import io.vertx.ext.asyncsql.MySQLClient;
import io.vertx.ext.asyncsql.PostgreSQLClient;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.ext.sql.SQLConnection;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.templ.HandlebarsTemplateEngine;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

public class App extends AbstractVerticle {

  /**
   * MongoDB implementation
   */
  private final class MongoDB {
    private final JsonObject FIELDS = new JsonObject().put("_id", 0);

    private final MongoClient database;
    // In order to use a template we first need to create an engine
    private final HandlebarsTemplateEngine engine;

    public MongoDB(Vertx vertx, JsonObject config) {
      this.database = MongoClient.createShared(vertx, config);
      this.engine = HandlebarsTemplateEngine.create();
    }

    public final void dbHandler(final RoutingContext ctx) {
      database.findOne("world", new JsonObject().put("id", Helper.randomWorld()), FIELDS, findOne -> {
        if (findOne.failed()) {
          ctx.fail(findOne.cause());
          return;
        }

        ctx.response()
            .putHeader(HttpHeaders.SERVER, SERVER)
            .putHeader(HttpHeaders.DATE, date)
            .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
            .end(new World(findOne.result()).encode());
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
                .end(new JsonArray(Arrays.asList(worlds)).encode());

          } else {

            final Handler<Integer> self = this;

            database.findOne("world", new JsonObject().put("id", Helper.randomWorld()), FIELDS, findOne -> {
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
                .end(new JsonArray(Arrays.asList(worlds)).encode());

          } else {

            final Handler<Integer> self = this;

            final int id = Helper.randomWorld();

            final JsonObject query = new JsonObject().put("id", id);

            database.findOne("world", query, FIELDS, findOne -> {
              if (findOne.failed()) {
                ctx.fail(findOne.cause());
                return;
              }

              final int newRandomNumber = Helper.randomWorld();

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
   * JDBC implementation
   */
  private final class AsyncSQL {

    public static final int MYSQL = 0;
    public static final int POSTGRES = 1;

    private final AsyncSQLClient database;
    private final int dbms;

    // In order to use a template we first need to create an engine
    private final HandlebarsTemplateEngine engine;

    public AsyncSQL(Vertx vertx, int driver, JsonObject config) {
      switch (driver) {
        case MYSQL:
          this.database = MySQLClient.createNonShared(vertx, config);
          this.dbms = MYSQL;
          break;
        case POSTGRES:
          this.database = PostgreSQLClient.createNonShared(vertx, config);
          this.dbms = POSTGRES;
          break;
        default:
          throw new RuntimeException("Unsupported DB");
      }
      this.engine = HandlebarsTemplateEngine.create();
    }

    public final void dbHandler(final RoutingContext ctx) {
      database.getConnection(getConnection -> {
        if (getConnection.failed()) {
          ctx.fail(getConnection.cause());
          return;
        }

        final SQLConnection conn = getConnection.result();

        conn.query("SELECT id, randomnumber from WORLD where id = " + Helper.randomWorld(), query -> {
          // free the connection
          conn.close();

          if (query.failed()) {
            ctx.fail(query.cause());
            return;
          }

          final List<JsonArray> resultSet = query.result().getResults();

          if (resultSet == null || resultSet.size() == 0) {
            ctx.fail(404);
            return;
          }

          final JsonArray row = resultSet.get(0);

          ctx.response()
              .putHeader(HttpHeaders.SERVER, SERVER)
              .putHeader(HttpHeaders.DATE, date)
              .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
              .end(new World(row.getInteger(0), row.getInteger(1)).encode());
        });
      });
    }

    public final void queriesHandler(final RoutingContext ctx) {
      final int queries = Helper.getQueries(ctx.request());
      final JsonArray worlds = new JsonArray();

      database.getConnection(getConnection -> {
        if (getConnection.failed()) {
          ctx.fail(getConnection.cause());
          return;
        }

        final SQLConnection conn = getConnection.result();

        new Handler<Integer>() {
          @Override
          public void handle(Integer idx) {
            if (idx == queries) {
              // stop condition
              ctx.response()
                  .putHeader(HttpHeaders.SERVER, SERVER)
                  .putHeader(HttpHeaders.DATE, date)
                  .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                  .end(worlds.encode());

              conn.close();
            } else {

              final Handler<Integer> self = this;

              conn.query("SELECT id, randomnumber from WORLD where id = " + Helper.randomWorld(), query -> {
                if (query.failed()) {
                  ctx.fail(query.cause());
                  conn.close();
                  return;
                }

                final List<JsonArray> resultSet = query.result().getResults();

                if (resultSet == null || resultSet.size() == 0) {
                  ctx.fail(404);
                  conn.close();
                  return;
                }

                final JsonArray row = resultSet.get(0);

                worlds.add(new World(row.getInteger(0), row.getInteger(1)));
                self.handle(idx + 1);
              });
            }
          }
        }.handle(0);
      });
    }

    public final void fortunesHandler(final RoutingContext ctx) {
      final List<Fortune> fortunes = new LinkedList<>();

      database.getConnection(getConnection -> {
        if (getConnection.failed()) {
          ctx.fail(getConnection.cause());
          return;
        }

        final SQLConnection conn = getConnection.result();

        conn.query("SELECT id, message from FORTUNE", query -> {
          // free the connection
          conn.close();

          if (query.failed()) {
            ctx.fail(query.cause());
            return;
          }

          final List<JsonArray> resultSet = query.result().getResults();

          if (resultSet == null || resultSet.size() == 0) {
            ctx.fail(404);
            return;
          }

          for (JsonArray row : resultSet) {
            fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
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
      });
    }

    public final void updateHandler(final RoutingContext ctx) {
      final int queries = Helper.getQueries(ctx.request());
      final JsonArray worlds = new JsonArray();

      final StringBuffer batch;

      if (dbms == POSTGRES) {
        // Postgres can batch queries
        batch = new StringBuffer();
      } else {
        batch = null;
      }

      database.getConnection(getConnection -> {
        if (getConnection.failed()) {
          ctx.fail(getConnection.cause());
          return;
        }

        final SQLConnection conn = getConnection.result();

        new Handler<Integer>() {
          @Override
          public void handle(Integer idx) {
            if (idx == queries) {
              switch (dbms) {
                case MYSQL:
                  ctx.response()
                      .putHeader(HttpHeaders.SERVER, SERVER)
                      .putHeader(HttpHeaders.DATE, date)
                      .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                      .end(worlds.encode());

                  conn.close();
                  break;
                case POSTGRES:
                  // stop condition, first run the batch update
                  conn.update(batch.toString(), update -> {
                    if (update.failed()) {
                      ctx.fail(update.cause());
                      conn.close();
                      return;
                    }
                    ctx.response()
                        .putHeader(HttpHeaders.SERVER, SERVER)
                        .putHeader(HttpHeaders.DATE, date)
                        .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                        .end(worlds.encode());

                    conn.close();
                  });
                  break;
              }
            } else {

              final Handler<Integer> self = this;
              final int id = Helper.randomWorld();

              conn.query("SELECT id, randomnumber from WORLD where id = " + id, query -> {
                if (query.failed()) {
                  ctx.fail(query.cause());
                  conn.close();
                  return;
                }

                final List<JsonArray> resultSet = query.result().getResults();

                if (resultSet == null || resultSet.size() == 0) {
                  ctx.fail(404);
                  conn.close();
                  return;
                }

                final int newRandomNumber = Helper.randomWorld();
                worlds.add(new World(id, newRandomNumber));

                switch (dbms) {
                  case MYSQL:
                    conn.update("UPDATE WORLD SET randomnumber = " + newRandomNumber + " WHERE id = " + id, update -> {
                      if (update.failed()) {
                        ctx.fail(update.cause());
                        conn.close();
                        return;
                      }

                      self.handle(idx + 1);
                    });
                    break;
                  case POSTGRES:
                    batch
                        .append("UPDATE WORLD SET randomnumber = ")
                        .append(newRandomNumber)
                        .append(" WHERE id = ")
                        .append(id)
                        .append("; ");

                    self.handle(idx + 1);
                    break;
                }
              });
            }
          }
        }.handle(0);
      });
    }
  }

  private static final String SERVER = "vertx-web";
  private String date;

  @Override
  public void start() {
    final Router app = Router.router(vertx);

    vertx.setPeriodic(1000, handler -> date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));

    final MongoDB mongoDB = new MongoDB(vertx, config());
    final AsyncSQL psql = new AsyncSQL(vertx, AsyncSQL.POSTGRES, config());
    final AsyncSQL mysql = new AsyncSQL(vertx, AsyncSQL.MYSQL, config());

    /**
     * This test exercises the framework fundamentals including keep-alive support, request routing, request header
     * parsing, object instantiation, JSON serialization, response header generation, and request count throughput.
     */
    app.get("/json").handler(ctx -> {
      ctx.response()
          .putHeader(HttpHeaders.SERVER, SERVER)
          .putHeader(HttpHeaders.DATE, date)
          .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
          .end(new Message("Hello, World!").encode());
    });

    /**
     * This test exercises the framework's object-relational mapper (ORM), random number generator, database driver,
     * and database connection pool.
     */
    app.get("/mongo/db").handler(mongoDB::dbHandler);
    app.get("/psql/db").handler(psql::dbHandler);
    app.get("/mysql/db").handler(mysql::dbHandler);

    /**
     * This test is a variation of Test #2 and also uses the World table. Multiple rows are fetched to more dramatically
     * punish the database driver and connection pool. At the highest queries-per-request tested (20), this test
     * demonstrates all frameworks' convergence toward zero requests-per-second as database activity increases.
     */
    app.get("/mongo/queries").handler(mongoDB::queriesHandler);
    app.get("/psql/queries").handler(psql::queriesHandler);
    app.get("/mysql/queries").handler(mysql::queriesHandler);

    /**
     * This test exercises the ORM, database connectivity, dynamic-size collections, sorting, server-side templates,
     * XSS countermeasures, and character encoding.
     */
    app.get("/mongo/fortunes").handler(mongoDB::fortunesHandler);
    app.get("/psql/fortunes").handler(psql::fortunesHandler);
    app.get("/mysql/fortunes").handler(mysql::fortunesHandler);

    /**
     * This test is a variation of Test #3 that exercises the ORM's persistence of objects and the database driver's
     * performance at running UPDATE statements or similar. The spirit of this test is to exercise a variable number of
     * read-then-write style database operations.
     */
    app.route("/mongo/update").handler(mongoDB::updateHandler);
    app.route("/psql/update").handler(psql::updateHandler);
    app.route("/mysql/update").handler(mysql::updateHandler);

    /**
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
