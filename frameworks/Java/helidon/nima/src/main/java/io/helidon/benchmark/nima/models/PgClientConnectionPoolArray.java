
package io.helidon.benchmark.nima.models;

import java.util.concurrent.locks.ReentrantLock;

import io.helidon.config.Config;
import io.vertx.core.Vertx;
import io.vertx.pgclient.PgConnectOptions;

class PgClientConnectionPoolArray extends PgClientConnectionPool {

    private final int connections;
    private final PgClientConnection[] connectionArray;
    private final ReentrantLock lock = new ReentrantLock();

    PgClientConnectionPoolArray(Vertx vertx, PgConnectOptions options, Config config) {
        super(vertx, options, config);
        double sizeFactor = config.get("pgclient-connection-pool.size-factor")
                .asDouble()
                .orElse(1.0);
        connections = (int) (Runtime.getRuntime().availableProcessors() * sizeFactor);
        connectionArray = new PgClientConnection[connections];
    }

    @Override
    public PgClientConnection clientConnection() {
        int index = Thread.currentThread().hashCode() % connections;
        PgClientConnection connection = connectionArray[index];
        if (connection == null) {
            try {
                lock.lock();
                connection = connectionArray[index];
                if (connection == null) {
                    connection = newConnection();
                    connectionArray[index] = connection;
                }
            } finally {
                lock.unlock();
            }
        }
        return connection;
    }

    @Override
    public void close() {
        try {
            for (PgClientConnection connection : connectionArray) {
                if (connection != null) {
                    connection.close();
                }
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
