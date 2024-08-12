package io.helidon.benchmark.nima.models;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Logger;

import io.helidon.config.Config;

import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.Future;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.PoolOptions;
import io.vertx.sqlclient.PreparedQuery;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.SqlClient;
import io.vertx.sqlclient.Tuple;

import static io.helidon.benchmark.nima.models.DbRepository.randomWorldNumber;

public class PgClientRepository implements DbRepository {
    private static final Logger LOGGER = Logger.getLogger(PgClientRepository.class.getName());

    private final SqlClient queryPool;
    private final SqlClient updatePool;

    private final int batchSize;
    private final long updateTimeout;
    private final int maxRetries;

    private final PreparedQuery<RowSet<Row>> getFortuneQuery;
    private final PreparedQuery<RowSet<Row>> getWorldQuery;
    private final PreparedQuery<RowSet<Row>> updateWorldQuery;

    public PgClientRepository(Config config) {
        Vertx vertx = Vertx.vertx(new VertxOptions()
                                          .setPreferNativeTransport(true));
        PgConnectOptions connectOptions = new PgConnectOptions()
                .setPort(config.get("port").asInt().orElse(5432))
                .setCachePreparedStatements(config.get("cache-prepared-statements").asBoolean().orElse(true))
                .setHost(config.get("host").asString().orElse("tfb-database"))
                .setDatabase(config.get("db").asString().orElse("hello_world"))
                .setUser(config.get("username").asString().orElse("benchmarkdbuser"))
                .setPassword(config.get("password").asString().orElse("benchmarkdbpass"));

        int sqlPoolSize = config.get("sql-pool-size").asInt().orElse(64);
        PoolOptions clientOptions = new PoolOptions().setMaxSize(sqlPoolSize);
        LOGGER.info("sql-pool-size is " + sqlPoolSize);
        batchSize = config.get("update-batch-size").asInt().orElse(20);
        LOGGER.info("update-batch-size is " + batchSize);
        updateTimeout = config.get("update-timeout-millis").asInt().orElse(5000);
        LOGGER.info("update-timeout-millis is " + updateTimeout);
        maxRetries = config.get("update-max-retries").asInt().orElse(3);
        LOGGER.info("update-max-retries is " + maxRetries);

        queryPool = PgPool.client(vertx, connectOptions, clientOptions);
        updatePool = PgPool.client(vertx, connectOptions, clientOptions);

        getWorldQuery = queryPool.preparedQuery("SELECT id, randomnumber FROM world WHERE id = $1");
        updateWorldQuery = queryPool.preparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2");
        getFortuneQuery = queryPool.preparedQuery("SELECT id, message FROM fortune");
    }

    @Override
    public World getWorld(int id) {
        try {
            return getWorldQuery.execute(Tuple.of(id))
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
            List<Future<?>> futures = new ArrayList<>();
            for (int i = 0; i < count; i++) {
                futures.add(getWorldQuery.execute(Tuple.of(randomWorldNumber()))
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
    public World updateWorld(World world) {
        try {
            return updateWorldQuery.execute(Tuple.of(world.id, world.id))
                    .toCompletionStage()
                    .thenApply(rows -> world)
                    .toCompletableFuture().get();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<World> updateWorlds(int count) {
        List<World> worlds = getWorlds(count);
        if (batchSize > 1) {        // batching updates
            for (World w : worlds) {
                w.randomNumber = randomWorldNumber();
            }
            if (count <= batchSize) {
                LOGGER.finest(() -> "Updating single batch of size " + count);
                updateWorldsRetry(worlds, 0, 0);
            } else {
                int batches = count / batchSize + (count % batchSize == 0 ? 0 : 1);
                for (int i = 0; i < batches; i++) {
                    final int from = i * batchSize;
                    LOGGER.finest(() -> "Updating batch from " + from + " to " + (from + batchSize));
                    updateWorldsRetry(worlds, from, 0);
                }
            }
        } else {                    // no batching for size 1
            for (World w : worlds) {
                w.randomNumber = randomWorldNumber();
                updateWorld(w);
            }
        }
        return worlds;
    }

    private List<World> updateWorldsRetry(List<World> worlds, int from, int retries) {
        if (retries > maxRetries) {
            throw new RuntimeException("Too many transaction retries");
        }
        CompletableFuture<List<World>> cf = null;
        try {
            cf = updateWorlds(worlds, from, updatePool);
            cf.get(updateTimeout, TimeUnit.MILLISECONDS);
            return worlds;
        } catch (ExecutionException | TimeoutException e) {
            cf.cancel(true);
            retries++;
            final int finalRetries = retries;
            LOGGER.fine(() -> "Retrying batch update after cancellation (retries=" + finalRetries + ")");
            return updateWorldsRetry(worlds, from, retries);     // retry
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<Fortune> getFortunes() {
        return getFortuneQuery.execute()
                .map(rows -> {
                    List<Fortune> fortunes = new ArrayList<>(rows.size() + 1);
                    for (Row r : rows) {
                        fortunes.add(new Fortune(r.getInteger(0), r.getString(1)));
                    }
                    return fortunes;
                }).result();
    }

    private CompletableFuture<List<World>> updateWorlds(List<World> worlds, int from, SqlClient pool) {
        List<Tuple> tuples = new ArrayList<>();
        int to = Math.min(from + batchSize, worlds.size());
        for (int i = from; i < to; i++) {
            World w = worlds.get(i);
            tuples.add(Tuple.of(w.randomNumber, w.id));
        }
        return updateWorldQuery.executeBatch(tuples)
                .toCompletionStage()
                .thenApply(rows -> worlds)
                .toCompletableFuture();
    }
}