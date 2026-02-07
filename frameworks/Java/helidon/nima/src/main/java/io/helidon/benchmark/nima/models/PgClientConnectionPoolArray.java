
package io.helidon.benchmark.nima.models;

import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Logger;

import io.helidon.config.Config;
import io.vertx.core.Vertx;
import io.vertx.pgclient.PgConnectOptions;

class PgClientConnectionPoolArray extends PgClientConnectionPool {
    private static final Logger LOGGER = Logger.getLogger(PgClientConnectionPoolArray.class.getName());

    private final int connections;
    private final PgClientConnection[] connectionArray;
    private final ReadWriteLock lock = new ReentrantReadWriteLock();

    PgClientConnectionPoolArray(Vertx vertx, PgConnectOptions options, Config config) {
        super(vertx, options, config);
        double sizeFactor = config.get("pgclient-connection-pool.size-factor")
                .asDouble()
                .orElse(1.0);
        connections = (int) (Runtime.getRuntime().availableProcessors() * sizeFactor);
        connectionArray = new PgClientConnection[connections];
        LOGGER.info("Connection pool is " + getClass().getSimpleName());
        LOGGER.info("Size of connection pool is " + connections);
    }

    @Override
    public PgClientConnection clientConnection() {
        int index = Thread.currentThread().hashCode() % connections;
        if (connectionArray[index] == null) {
            lock.readLock().lock();
            if (connectionArray[index] == null) {
                lock.readLock().unlock();
                lock.writeLock().lock();
                try {
                    if (connectionArray[index] == null) {
                        connectionArray[index] = newConnection();
                    }
                } finally {
                    lock.writeLock().unlock();
                }
            } else {
                lock.readLock().unlock();
            }
        }
        return connectionArray[index];
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
