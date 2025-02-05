
package io.helidon.benchmark.nima.models;

import java.util.concurrent.locks.ReentrantLock;

import io.vertx.core.Vertx;
import io.vertx.pgclient.PgConnectOptions;

class PgClientConnectionPoolArray extends PgClientConnectionPool {

    private final ReentrantLock lock = new ReentrantLock();
    private final int connections = Runtime.getRuntime().availableProcessors() * 2;
    private final PgClientConnection[] connectionArray = new PgClientConnection[connections];

    PgClientConnectionPoolArray(Vertx vertx, PgConnectOptions options) {
        super(vertx, options);
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
