package io.helidon.benchmark.nima.models;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import io.helidon.config.Config;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.sqlclient.PreparedQuery;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;

import static io.helidon.benchmark.nima.models.DbRepository.randomWorldNumber;
import static io.helidon.benchmark.nima.models.PgClientConnectionPool.PgClientConnection.UPDATE_QUERIES;

public class PgClientRepository implements DbRepository {
    private static final Logger LOGGER = Logger.getLogger(PgClientRepository.class.getName());

    private final PgClientConnectionPool connectionPool;

    @SuppressWarnings("unchecked")
    public PgClientRepository(Config config) {
        VertxOptions vertxOptions = new VertxOptions()
                .setPreferNativeTransport(true)
                .setBlockedThreadCheckInterval(100000);
        Vertx vertx = Vertx.vertx(vertxOptions);
        PgConnectOptions connectOptions = new PgConnectOptions()
                .setPort(config.get("port").asInt().orElse(5432))
                .setHost(config.get("host").asString().orElse("tfb-database"))
                .setDatabase(config.get("db").asString().orElse("hello_world"))
                .setUser(config.get("username").asString().orElse("benchmarkdbuser"))
                .setPassword(config.get("password").asString().orElse("benchmarkdbpass"))
                .setCachePreparedStatements(true)
                .setPreparedStatementCacheMaxSize(UPDATE_QUERIES + 2)
                .setPreparedStatementCacheSqlFilter(s -> true)          // cache all
                .setTcpNoDelay(true)
                .setTcpQuickAck(true)
                .setTcpKeepAlive(true)
                .setPipeliningLimit(100000);
        connectionPool = new PgClientConnectionPool(vertx, connectOptions);
    }

    @Override
    public World getWorld(int id) {
        try {
            PreparedQuery<RowSet<Row>> worldQuery = connectionPool.clientConnection().worldQuery();
            return worldQuery.execute(Tuple.of(id))
                    .map(rows -> {
                        Row r = rows.iterator().next();
                        return new World(r.getInteger(0), r.getInteger(1));
                    }).toCompletionStage().toCompletableFuture().get();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<World> getWorlds(int count) {
        try {
            PreparedQuery<RowSet<Row>> worldQuery = connectionPool.clientConnection().worldQuery();
            List<Future<?>> futures = new ArrayList<>();
            for (int i = 0; i < count; i++) {
                futures.add(worldQuery.execute(Tuple.of(randomWorldNumber()))
                        .map(rows -> {
                            Row r = rows.iterator().next();
                            return new World(r.getInteger(0), r.getInteger(1));
                        }));
            }
            return Future.all(futures).toCompletionStage().toCompletableFuture().get().list();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<World> updateWorlds(int count) {
        List<World> worlds = getWorlds(count);
        try {
            PreparedQuery<RowSet<Row>> updateQuery = connectionPool.clientConnection().updateQuery(count);
            List<Integer> updateParams = new ArrayList<>(count * 2);
            for (World world : worlds) {
                updateParams.add(world.id);
                world.randomNumber = randomWorldNumber();
                updateParams.add(world.randomNumber);
            }
            return updateQuery.execute(Tuple.wrap(updateParams))
                    .toCompletionStage()
                    .thenApply(rows -> worlds)
                    .toCompletableFuture()
                    .get();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<Fortune> getFortunes() {
        try {
            PreparedQuery<RowSet<Row>> fortuneQuery = connectionPool.clientConnection().fortuneQuery();
            return fortuneQuery.execute()
                    .map(rows -> {
                        List<Fortune> fortunes = new ArrayList<>(rows.size() + 1);
                        for (Row r : rows) {
                            fortunes.add(new Fortune(r.getInteger(0), r.getString(1)));
                        }
                        return fortunes;
                    }).toCompletionStage().toCompletableFuture().get();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}