
package io.helidon.benchmark.nima.models;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

import io.vertx.core.Vertx;
import io.vertx.pgclient.PgConnectOptions;

class PgClientConnectionPoolMap extends PgClientConnectionPool {

    private final ReentrantLock lock = new ReentrantLock();
    private final Map<String, PgClientConnection> connectionMap = new HashMap<>();

    PgClientConnectionPoolMap(Vertx vertx, PgConnectOptions options) {
        super(vertx, options);
    }

    @Override
    public PgClientConnection clientConnection() {
        String carrierThread = carrierThread();
        PgClientConnection connection = connectionMap.get(carrierThread);
        if (connection == null) {
            try {
                lock.lock();
                connection = connectionMap.get(carrierThread);
                if (connection == null) {
                    connection = newConnection();
                    connectionMap.put(carrierThread, connection);
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
            for (PgClientConnection connection : connectionMap.values()) {
                connection.close();
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    static String carrierThread() {
        String threadName = Thread.currentThread().toString();
        return threadName.substring(threadName.indexOf('@') + 1);
    }
}
