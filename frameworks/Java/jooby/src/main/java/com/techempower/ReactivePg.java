package com.techempower;

import static io.jooby.ExecutionMode.EVENT_LOOP;
import static io.jooby.MediaType.JSON;
import static io.jooby.Reified.getParameterized;
import static java.util.stream.IntStream.range;

import java.util.*;
import java.util.function.Consumer;

import io.jooby.*;
import io.jooby.rocker.RockerModule;
import io.jooby.vertx.pgclient.VertxPgConnectionModule;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.sqlclient.*;
import io.vertx.sqlclient.impl.SqlClientInternal;

public class ReactivePg extends Jooby {
    private final PreparedQuery<RowSet<Row>> selectWorldQuery;
    private final PreparedQuery<RowSet<Row>> selectFortuneQuery;
    private final List<PreparedQuery<RowSet<Row>>> updateWorldQuery;
    private final SqlClientInternal sqlClient;

    {
        /** PG client: */
        install(new VertxPgConnectionModule().prepare(statements()));

        /** Template engine: */
        install(new RockerModule());
        Json.configure(getOutputFactory());

        this.selectWorldQuery = require(PreparedQueryType, "selectWorld");
        this.selectFortuneQuery = require(PreparedQueryType, "selectFortune");
        this.updateWorldQuery = require(PreparedQueryTypeList, "updateWorld");
        this.sqlClient = require(SqlClientInternal.class);

        /** Single query: */
        get("/db", ctx -> {
            selectWorldQuery
                    .execute(Tuple.of(Util.boxedRandomWorld()))
                    .onComplete(
                            rsp -> {
                                if (rsp.succeeded()) {
                                    var rs = rsp.result().iterator();
                                    var row = rs.next();
                                    ctx.setResponseType(JSON)
                                            .send(Json.encode(new World(row.getInteger(0), row.getInteger(1))));
                                } else {
                                    ctx.sendError(rsp.cause());
                                }
                            });
            return ctx;
        });

        /** Multiple queries: */
        get("/queries", ctx -> {
            int queries = Util.queries(ctx);
            selectWorlds(ctx, queries, result -> ctx.setResponseType(JSON).send(Json.encode(result)));
            return ctx;
        });

        /** Update queries: */
        get("/updates", ctx -> {
            int queries = Util.queries(ctx);
            selectWorlds(
                    ctx,
                    queries,
                    result -> {
                        updateWorld(
                                result,
                                ar -> {
                                    if (ar.failed()) {
                                        sendError(ctx, ar.cause());
                                    } else {
                                        ctx.setResponseType(JSON).send(Json.encode(result));
                                    }
                                });
                    });
            return ctx;
        });

        /** Fortunes: */
        get("/fortunes", ctx -> {
            selectFortuneQuery.execute()
                    .onComplete(rsp -> {
                        if (rsp.succeeded()) {
                            RowIterator<Row> rs = rsp.result().iterator();
                            List<Fortune> fortunes = new ArrayList<>();

                            while (rs.hasNext()) {
                                Row row = rs.next();
                                fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
                            }

                            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                            Collections.sort(fortunes);

                            /** render view: */
                            ctx.setResponseType(MediaType.html)
                                    .render(views.fortunes.template(fortunes));
                        } else {
                            sendError(ctx, rsp.cause());
                        }
                    });
            return ctx;
        });
    }

    private void selectWorlds(Context ctx, int queries, Consumer<List<World>> consumer) {
        sqlClient.group(
                client -> {
                    List<World> worlds = new ArrayList<>(queries);
                    for (int i = 0; i < queries; i++) {
                        client.preparedQuery(SELECT_WORLD)
                                .execute(Tuple.of(Util.boxedRandomWorld()))
                                .map(rs -> new World(rs.iterator().next().getInteger(0), Util.boxedRandomWorld()))
                                .onComplete(
                                        ar -> {
                                            if (ar.succeeded()) {
                                                worlds.add(ar.result());
                                                if (worlds.size() == queries) {
                                                    consumer.accept(worlds);
                                                }
                                            } else {
                                                sendError(ctx, ar.cause());
                                            }
                                        });
                    }
                });
    }

    private void updateWorld(List<World> worlds, Handler<AsyncResult<RowSet<Row>>> handler) {
        Collections.sort(worlds);
        int len = worlds.size();
        List<Object> arguments = new ArrayList<>();
        for (var world : worlds) {
            arguments.add(world.getId());
            arguments.add(world.getRandomNumber());
        }
        updateWorldQuery.get(len - 1).execute(Tuple.wrap(arguments)).onComplete(handler);
    }

    private void sendError(Context ctx, Throwable cause) {
        if (!ctx.isResponseStarted()) {
            ctx.sendError(cause);
        }
    }

    private Map<String, List<String>> statements() {
        return Map.of(
                "selectWorld", List.of(SELECT_WORLD),
                "selectFortune", List.of(SELECT_FORTUNE),
                "updateWorld",
                range(0, 500).map(i -> i + 1).mapToObj(this::buildAggregatedUpdateQuery).toList());
    }

    private String buildAggregatedUpdateQuery(int len) {
        var sql = new StringBuilder();
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

    private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    private static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";

    private static final Reified<PreparedQuery<RowSet<Row>>> PreparedQueryType =
            getParameterized(PreparedQuery.class, getParameterized(RowSet.class, Row.class));

    private static final Reified<List<PreparedQuery<RowSet<Row>>>> PreparedQueryTypeList =
            Reified.list(PreparedQueryType);

    public static void main(String[] args) {
        runApp(args, EVENT_LOOP, ReactivePg::new);
    }
}
