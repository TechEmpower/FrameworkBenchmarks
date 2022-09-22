package io.helidon.benchmark.nima.models;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Logger;

import io.helidon.common.reactive.Multi;
import io.helidon.common.reactive.Single;
import io.helidon.config.Config;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.PoolOptions;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.SqlClient;
import io.vertx.sqlclient.Tuple;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;

import static io.helidon.benchmark.nima.models.DbRepository.randomWorldNumber;

public class PgClientRepository implements DbRepository {
    private static final Logger LOGGER = Logger.getLogger(PgClientRepository.class.getName());


    private final SqlClient queryPool;
    private final SqlClient updatePool;

    private final int batchSize;
    private final long updateTimeout;
    private final int maxRetries;

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
    }

    @Override
    public JsonObject getWorldAsJson(int id) {
        return getWorld(id, queryPool).map(World::toJson).await();
    }

    @Override
    public World getWorld(int id) {
        try {
            return getWorld(id, queryPool).toCompletableFuture().get();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public JsonArray getWorldsAsJson(int count) {
        try {
            return Multi.range(0, count)
                    .flatMap(i -> getWorld(randomWorldNumber(), queryPool))
                    .map(World::toJson)
                    .reduce(JSON::createArrayBuilder, JsonArrayBuilder::add)
                    .map(JsonArrayBuilder::build)
                    .await();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<World> getWorlds(int count) {
        try {
            List<World> result = new ArrayList<>(count);
            for (int i = 0; i < count; i++) {
                World world = queryPool.preparedQuery("SELECT id, randomnumber FROM world WHERE id = $1")
                        .execute(Tuple.of(randomWorldNumber()))
                        .map(rows -> {
                            Row r = rows.iterator().next();
                            return new World(r.getInteger(0), r.getInteger(1));
                        }).toCompletionStage().toCompletableFuture().get();
                result.add(world);
            }
            return result;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public World updateWorld(World world) {
        try {
            updateWorlds(List.of(world), 0, updatePool);
            return world;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<World> updateWorlds(int count) {
        List<World> worlds = getWorlds(count);
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
        try {
            return queryPool.preparedQuery("SELECT id, message FROM fortune")
                    .execute()
                    .map(rows -> {
                        List<Fortune> fortunes = new ArrayList<>(rows.size() + 1);
                        for (Row r : rows) {
                            fortunes.add(new Fortune(r.getInteger(0), r.getString(1)));
                        }
                        return fortunes;
                    })
                    .toCompletionStage().toCompletableFuture().get();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static Single<World> getWorld(int id, SqlClient pool) {
        return Single.create(pool.preparedQuery("SELECT id, randomnumber FROM world WHERE id = $1")
                .execute(Tuple.of(id))
                .map(rows -> {
                    Row r = rows.iterator().next();
                    return new World(r.getInteger(0), r.getInteger(1));
                }).toCompletionStage());

    }

    private CompletableFuture<List<World>> updateWorlds(List<World> worlds, int from, SqlClient pool) {
        List<Tuple> tuples = new ArrayList<>();
        int to = Math.min(from + batchSize, worlds.size());
        for (int i = from; i < to; i++) {
            World w = worlds.get(i);
            tuples.add(Tuple.of(w.randomNumber, w.id));
        }
        return pool.preparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2")
                .executeBatch(tuples)
                .toCompletionStage()
                .thenApply(rows -> worlds)
                .toCompletableFuture();
    }
}