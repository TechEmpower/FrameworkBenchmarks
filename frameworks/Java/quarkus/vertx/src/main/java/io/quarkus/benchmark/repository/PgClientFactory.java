package io.quarkus.benchmark.repository;

import io.vertx.core.Vertx;
import io.vertx.pgclient.PgConnectOptions;
import jakarta.annotation.PreDestroy;
import jakarta.enterprise.inject.Produces;
import jakarta.inject.Singleton;
import org.eclipse.microprofile.config.inject.ConfigProperty;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

    private final Vertx vertx;
    private PgConnectionPool pgConnectionPool;

    public PgClientFactory(final Vertx vertx) {
        this.vertx = vertx;
    }

    @Produces
    @Singleton
    PgConnectionPool connectionPool() {
        PgConnectionPool pgConnectionPool = null;
        try {
            pgConnectionPool = new PgConnectionPool(vertx, pgConnectOptions());
        } catch (final Exception e) {
            // TODO LOG ME: usually means inability to connect to the database
        } finally {
            this.pgConnectionPool = pgConnectionPool;
            return pgConnectionPool;
        }
    }

    @PreDestroy
    public void closeConnectionPool() {
        if (pgConnectionPool != null) {
            pgConnectionPool.close();
        }
    }

    private PgConnectOptions pgConnectOptions() {
        final PgConnectOptions options = new PgConnectOptions();
        final Matcher matcher = Pattern.compile(PG_URI_MATCHER).matcher(url);
        matcher.matches();
        options.setDatabase(matcher.group(3));
        options.setHost(matcher.group(1));
        options.setPort(Integer.parseInt(matcher.group(2)));
        options.setUser(user);
        options.setPassword(pass);
        options.setCachePreparedStatements(true);
        // Large pipelining means less flushing and we use a single connection anyway
        options.setPipeliningLimit(100_000);
        return options;
    }
}