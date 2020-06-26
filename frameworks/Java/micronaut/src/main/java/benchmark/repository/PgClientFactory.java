package benchmark.repository;

import io.micronaut.context.annotation.Bean;
import io.micronaut.context.annotation.Factory;
import io.reactiverse.pgclient.PgClient;
import io.reactiverse.pgclient.PgPool;
import io.reactiverse.pgclient.PgPoolOptions;
import io.vertx.core.Vertx;

import javax.inject.Singleton;
import java.util.ArrayList;
import java.util.List;

@Factory
public class PgClientFactory {

    private final PgClientConfig config;

    public PgClientFactory(PgClientConfig config) {
        this.config = config;
    }

    @Bean
    @Singleton
    public Vertx vertx() {
        return Vertx.vertx();
    }

    @Bean
    @Singleton
    public PgClients pgClients(Vertx vertx) {
        List<PgClient> clients = new ArrayList<>();

        for (int i = 0; i < Runtime.getRuntime().availableProcessors(); i++) {
            clients.add(pgClient(vertx));
        }

        return new PgClients(clients);
    }


    private PgPool pgClient(Vertx vertx) {
        PgPoolOptions options = new PgPoolOptions();
        options.setDatabase(config.getName());
        options.setHost(config.getHost());
        options.setPort(config.getPort());
        options.setUser(config.getUsername());
        options.setPassword(config.getPassword());
        options.setCachePreparedStatements(true);
        options.setMaxSize(1);
        return PgClient.pool(vertx, options);
    }
}
