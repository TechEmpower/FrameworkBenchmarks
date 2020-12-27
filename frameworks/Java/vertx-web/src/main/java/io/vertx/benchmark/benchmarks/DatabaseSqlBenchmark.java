package io.vertx.benchmark.benchmarks;

import com.github.susom.database.Config;
import com.github.susom.database.ConfigFrom;
import com.github.susom.database.DatabaseProviderVertx;
import com.github.susom.database.SqlInsert;
import io.vertx.benchmark.Helper;
import io.vertx.benchmark.model.Fortune;
import io.vertx.benchmark.model.World;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.templ.rocker.RockerTemplateEngine;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static io.vertx.benchmark.Helper.randomWorld;

/**
 * Implementation using com.github.susom:database library and standard JDBC driver.
 */
public final class DatabaseSqlBenchmark extends BaseBenchmark implements Benchmark{

    private final DatabaseProviderVertx.Builder dbBuilder;

    // In order to use a template we first need to create an engine
    private final RockerTemplateEngine engine;

    public DatabaseSqlBenchmark(Vertx vertx, JsonObject jsonConfig) {

        super(vertx);

        Config config = ConfigFrom.firstOf().custom(jsonConfig::getString)
                .rename("username", "database.user")
                .rename("password", "database.password")
                .rename("maxPoolSize", "database.pool.size")
                .value("database.url", "jdbc:postgresql://" + jsonConfig.getString("host") + "/" + jsonConfig.getString("database"))
                .get();
        dbBuilder = DatabaseProviderVertx.pooledBuilder(vertx, config);
        engine = RockerTemplateEngine.create();// initialize the date header

    }

    @Override
    public void dbHandler(final RoutingContext ctx) {
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

    @Override
    public void queriesHandler(final RoutingContext ctx) {
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

    @Override
    public void fortunesHandler(final RoutingContext ctx) {
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
                }
            } else {
                ctx.fail(call.cause());
            }
        });
    }

    @Override
    public void updateHandler(final RoutingContext ctx) {
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
