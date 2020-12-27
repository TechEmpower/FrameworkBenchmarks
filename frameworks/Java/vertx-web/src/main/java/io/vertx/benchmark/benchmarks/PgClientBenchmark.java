package io.vertx.benchmark.benchmarks;

import io.vertx.benchmark.Helper;
import io.vertx.benchmark.model.Fortune;
import io.vertx.benchmark.model.World;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.templ.rocker.RockerTemplateEngine;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.PoolOptions;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.Tuple;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static io.vertx.benchmark.Helper.randomWorld;

/**
 * PgClient implementation
 */
public final class PgClientBenchmark extends BaseBenchmark implements Benchmark {

    private static final String SERVER = "vertx-web";
    private String date;

    private static final String UPDATE_WORLD = "UPDATE world SET randomnumber=$1 WHERE id=$2";
    private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    private static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";

    private final PgPool client;

    // In order to use a template we first need to create an engine
    private final RockerTemplateEngine engine;

    public PgClientBenchmark(Vertx vertx, JsonObject config) {

        super(vertx);

        PgConnectOptions options = new PgConnectOptions()
                .setCachePreparedStatements(true)
                .setHost(config.getString("host"))
                .setPort(config.getInteger("port", 5432))
                .setUser(config.getString("username"))
                .setPassword(config.getString("password"))
                .setDatabase(config.getString("database"));

        client = PgPool.pool(vertx, options, new PoolOptions().setMaxSize(4));
        this.engine = RockerTemplateEngine.create();

    }

    @Override
    public final void dbHandler(final RoutingContext ctx) {
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

    @Override
    public final void queriesHandler(final RoutingContext ctx) {

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

    @Override
    public final void fortunesHandler(final RoutingContext ctx) {

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

    @Override
    public final void updateHandler(final RoutingContext ctx) {

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
