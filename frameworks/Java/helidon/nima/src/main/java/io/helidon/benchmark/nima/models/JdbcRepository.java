package io.helidon.benchmark.nima.models;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;

import io.helidon.config.Config;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.PoolOptions;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.SqlClient;
import io.vertx.sqlclient.Tuple;

import static io.helidon.benchmark.nima.models.DbRepository.randomWorldNumber;

public class JdbcRepository implements DbRepository {

    private final SqlClient pool;

    public JdbcRepository(Config config) {
        Vertx vertx = Vertx.vertx(new VertxOptions()
                .setPreferNativeTransport(true));
        PgConnectOptions connectOptions = new PgConnectOptions()
                .setPort(config.get("port").asInt().orElse(5432))
                .setCachePreparedStatements(config.get("cache-prepared-statements").asBoolean().orElse(true))
                .setHost(config.get("host").asString().orElse("tfb-database"))
                .setDatabase(config.get("db").asString().orElse("hello_world"))
                .setUser(config.get("username").asString().orElse("benchmarkdbuser"))
                .setPassword(config.get("password").asString().orElse("benchmarkdbpass"));
        PoolOptions clientOptions = new PoolOptions()
                .setMaxSize(config.get("sql-pool-size").asInt().orElse(64));
        pool = PgPool.client(vertx, connectOptions, clientOptions);
    }

    @Override
    public World getWorld(int id) {
        try {
            return getWorld(id, pool);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<World> getWorlds(int count) {
        try {
            List<World> result = new ArrayList<>(count);
            for (int i = 0; i < count; i++) {
                World world = pool.preparedQuery("SELECT id, randomnumber FROM world WHERE id = $1")
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
            return updateWorld(world, pool);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<World> updateWorlds(int count) {
        try {
            List<World> result = new ArrayList<>(count);
            for (int i = 0; i < count; i++) {
                World world = getWorld(randomWorldNumber(), pool);
                world.randomNumber = randomWorldNumber();
                result.add(updateWorld(world, pool));
            }
            return result;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<Fortune> getFortunes() {
        try {
            return pool.preparedQuery("SELECT id, message FROM fortune")
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

    private static World getWorld(int id, SqlClient client) throws ExecutionException, InterruptedException {
        return client.preparedQuery("SELECT id, randomnumber FROM world WHERE id = $1")
                .execute(Tuple.of(id))
                .map(rows -> {
                    Row r = rows.iterator().next();
                    return new World(r.getInteger(0), r.getInteger(1));
                }).toCompletionStage().toCompletableFuture().get();

    }

    private static World updateWorld(World world, SqlClient client) throws ExecutionException, InterruptedException {
        return client.preparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2")
                .execute(Tuple.of(world.randomNumber, world.id))
                .toCompletionStage()
                .thenApply(rows -> world)
                .toCompletableFuture().get();
    }
}