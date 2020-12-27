package io.vertx.benchmark.benchmarks;

import io.vertx.benchmark.Helper;
import io.vertx.benchmark.model.Fortune;
import io.vertx.benchmark.model.World;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.templ.rocker.RockerTemplateEngine;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import static io.vertx.benchmark.Helper.randomWorld;

/**
 * MongoDB implementation
 */
public final class MongoDBBenchmark extends BaseBenchmark implements Benchmark{

    private static final String SERVER = "vertx-web";
    private String date;

    private final JsonObject FIELDS = new JsonObject().put("_id", 0);

    private final MongoClient database;
    // In order to use a template we first need to create an engine
    private final RockerTemplateEngine engine;

    public MongoDBBenchmark(Vertx vertx, JsonObject config) {

        super(vertx);

        final JsonObject mongoConfig = config.copy();

        // mongo is configured without credentials
        mongoConfig.remove("username");
        mongoConfig.remove("password");

        this.database = MongoClient.createShared(vertx, mongoConfig);
        this.engine = RockerTemplateEngine.create();

    }

    @Override
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

    @Override
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

    @Override
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
        });
    }

    @Override
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

                        database.updateCollection("world", query, new JsonObject().put("$set", new JsonObject().put("randomNumber", newRandomNumber)), update -> {
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
