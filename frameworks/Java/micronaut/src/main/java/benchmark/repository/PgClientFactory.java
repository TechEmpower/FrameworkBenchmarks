package benchmark.repository;

import io.micronaut.context.annotation.Factory;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.reactivex.core.Vertx;
import io.vertx.reactivex.pgclient.PgPool;
import io.vertx.sqlclient.PoolOptions;

import javax.inject.Singleton;
import java.util.ArrayList;
import java.util.List;

@Factory
public class PgClientFactory {

    private final PgClientConfig config;

    public PgClientFactory(PgClientConfig config) {
        this.config = config;
    }

    @Singleton
    public Vertx vertx() {
        return Vertx.vertx();
    }

    @Singleton
    public PgClients pgClients(Vertx vertx) {
        List<PgPool> clients = new ArrayList<>();

        for (int i = 0; i < Runtime.getRuntime().availableProcessors(); i++) {
            clients.add(pgClient(vertx));
        }

        return new PgClients(clients);
    }


    private PgPool pgClient(Vertx vertx) {
        PgConnectOptions connectOptions = new PgConnectOptions();
        connectOptions.setDatabase(config.getName());
        connectOptions.setHost(config.getHost());
        connectOptions.setPort(config.getPort());
        connectOptions.setUser(config.getUsername());
        connectOptions.setPassword(config.getPassword());
        connectOptions.setCachePreparedStatements(true);
        PoolOptions poolOptions = new PoolOptions();
        poolOptions.setMaxSize(1);
        return PgPool.pool(vertx, connectOptions, poolOptions);
    }
}
