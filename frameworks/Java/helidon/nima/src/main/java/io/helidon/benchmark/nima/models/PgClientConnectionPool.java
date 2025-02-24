
package io.helidon.benchmark.nima.models;

import io.helidon.config.Config;
import io.vertx.core.Vertx;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgConnection;

abstract class PgClientConnectionPool implements AutoCloseable {

    private final Config config;
    private final Vertx vertx;
    private final PgConnectOptions options;

    static PgClientConnectionPool create(Vertx vertx, PgConnectOptions options, Config config) {
        return new PgClientConnectionPoolArray(vertx, options, config);
    }

    PgClientConnectionPool(Vertx vertx, PgConnectOptions options, Config config) {
        this.vertx = vertx;
        this.options = options;
        this.config = config;
    }

    abstract PgClientConnection clientConnection();

    protected PgClientConnection newConnection() {
        try {
            PgConnection conn = PgConnection.connect(vertx, options)
                    .toCompletionStage().toCompletableFuture().get();
            PgClientConnection clientConn = new PgClientConnection(conn);
            clientConn.prepare();
            return clientConn;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
