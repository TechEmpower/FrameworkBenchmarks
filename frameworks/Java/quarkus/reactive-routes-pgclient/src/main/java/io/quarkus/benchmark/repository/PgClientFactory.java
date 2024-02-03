package io.quarkus.benchmark.repository;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import io.vertx.mutiny.sqlclient.SqlClient;
import jakarta.enterprise.inject.Produces;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;
import org.eclipse.microprofile.config.inject.ConfigProperty;

import io.vertx.mutiny.core.Vertx;
import io.vertx.mutiny.pgclient.PgPool;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.sqlclient.PoolOptions;

@Singleton
public class PgClientFactory {

    // vertx-reactive:postgresql://tfb-database:5432/hello_world
    private static final String PG_URI_MATCHER = "vertx-reactive:postgresql://([-a-zA-Z]+):([0-9]+)/(.*)";

    @ConfigProperty(name = "quarkus.datasource.url")
    String url;

    @ConfigProperty(name = "quarkus.datasource.username")
    String user;

    @ConfigProperty(name = "quarkus.datasource.password")
    String pass;

    @Inject
    Vertx vertx;

    @Produces
    @Singleton
    public PgClients pgClients() {
        return new PgClients(this);
    }


    SqlClient sqlClient(final int size) {
        final PoolOptions options = new PoolOptions();
        final PgConnectOptions connectOptions = new PgConnectOptions();
        final Matcher matcher = Pattern.compile(PG_URI_MATCHER).matcher(url);
        matcher.matches();
        connectOptions.setDatabase(matcher.group(3));
        connectOptions.setHost(matcher.group(1));
        connectOptions.setPort(Integer.parseInt(matcher.group(2)));
        connectOptions.setUser(user);
        connectOptions.setPassword(pass);
        connectOptions.setCachePreparedStatements(true);
        // Large pipelining means less flushing and we use a single connection anyway
        connectOptions.setPipeliningLimit(100_000);
        options.setMaxSize(size);
        return PgPool.client(vertx, connectOptions, options);
    }
}