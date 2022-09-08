package io.helidon.benchmark.nima.models;

import java.util.ArrayList;
import java.util.List;

import io.helidon.config.Config;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.PoolOptions;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.SqlClient;
import io.vertx.sqlclient.Tuple;

public class JdbcRepository implements DbRepository {
    private Vertx vertx;
    private PgConnectOptions connectOptions;
    private PoolOptions clientOptions;
    private PoolOptions poolOptions;

    public JdbcRepository(Config config) {
        vertx = Vertx.vertx(new VertxOptions()
                .setPreferNativeTransport(true));
        connectOptions = new PgConnectOptions()
                .setPort(config.get("port").asInt().orElse(5432))
                .setCachePreparedStatements(config.get("cache-prepared-statements").asBoolean().orElse(true))
                .setHost(config.get("host").asString().orElse("tfb-database"))
                .setDatabase(config.get("db").asString().orElse("hello_world"))
                .setUser(config.get("username").asString().orElse("benchmarkdbuser"))
                .setPassword(config.get("password").asString().orElse("benchmarkdbpass"));

        clientOptions = new PoolOptions().setMaxSize(10);
        poolOptions = new PoolOptions().setMaxSize(40);
    }

    @Override
    public World getWorld(int id) {
        try {
            SqlClient client = PgPool.client(vertx, connectOptions, clientOptions);
            World world = client.preparedQuery("SELECT id, randomnumber FROM world WHERE id = $1")
                    .execute(Tuple.of(id))
                    .map(rows -> {
                        Row r = rows.iterator().next();
                        return new World(r.getInteger(0), r.getInteger(1));
                    }).toCompletionStage().toCompletableFuture().get();
            client.close().toCompletionStage().toCompletableFuture().get();
            return world;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public World updateWorld(World world) {
        try {
            SqlClient pool = PgPool.pool(vertx, connectOptions, poolOptions);
            World result = pool.preparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2")
                    .execute(Tuple.of(world.id, world.id))
                    .toCompletionStage()
                    .thenApply(rows -> world)
                    .toCompletableFuture().get();
            pool.close().toCompletionStage().toCompletableFuture().get();
            return result;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<Fortune> getFortunes() {
        try {
            SqlClient client = PgPool.client(vertx, connectOptions, clientOptions);
            List<Fortune> result = client.preparedQuery("SELECT id, message FROM fortune")
                    .execute()
                    .map(rows -> {
                        List<Fortune> fortunes = new ArrayList<>(rows.size() + 1);
                        for (Row r : rows) {
                            fortunes.add(new Fortune(r.getInteger(0), r.getString(1)));
                        }
                        return fortunes;
                    })
                    .toCompletionStage().toCompletableFuture().get();
            client.close().toCompletionStage().toCompletableFuture().get();
            return result;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}