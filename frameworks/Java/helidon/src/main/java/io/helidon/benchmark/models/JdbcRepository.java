package io.helidon.benchmark.models;

import java.util.ArrayList;
import java.util.List;

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

public class JdbcRepository implements DbRepository {
    private Vertx vertx;
    private PgConnectOptions connectOptions;
    private PoolOptions clientOptions;
    private PoolOptions poolOptions;

    private final ThreadLocal<SqlClient> client = ThreadLocal.withInitial(() -> PgPool.client(vertx, connectOptions, clientOptions));
    private final ThreadLocal<SqlClient> pool = ThreadLocal.withInitial(() -> PgPool.pool(vertx, connectOptions, poolOptions));

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

        clientOptions = new PoolOptions().setMaxSize(1);
        poolOptions = new PoolOptions().setMaxSize(4);
    }

    @Override
    public Single<World> getWorld(int id) {
        return Single.create(client.get().preparedQuery("SELECT id, randomnumber FROM world WHERE id = $1")
                .execute(Tuple.of(id))
                .map(rows -> {
                    Row r = rows.iterator().next();
                    return new World(r.getInteger(0), r.getInteger(1));
                })
                .toCompletionStage());
    }

    @Override
    public Single<World> updateWorld(World world) {
        return Single.create(pool.get().preparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2")
                .execute(Tuple.of(world.id, world.id))
                .toCompletionStage()
                .thenApply(rows -> world));
    }

    @Override
    public Single<List<Fortune>> getFortunes() {
        return Single.create(client.get().preparedQuery("SELECT id, message FROM fortune")
                .execute()
                .map(rows -> {
                    List<Fortune> fortunes = new ArrayList<>(rows.size() + 1);
                    for (Row r : rows) {
                        fortunes.add(new Fortune(r.getInteger(0), r.getString(1)));
                    }
                    return fortunes;
                })
                .toCompletionStage());
    }
}