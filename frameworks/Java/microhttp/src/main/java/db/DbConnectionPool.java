package db;

import java.sql.SQLException;
import java.time.Duration;
import java.util.concurrent.ConcurrentLinkedQueue;

public class DbConnectionPool {

    private final ConcurrentLinkedQueue<DbConnection> connections;

    private final int initialSize;
    private final Duration maxIdle;

    public DbConnectionPool(int initialSize, Duration maxIdle) {
        this.connections = new ConcurrentLinkedQueue<>();
        this.initialSize = initialSize;
        this.maxIdle = maxIdle;
    }

    public void start() {
        startDaemon(this::initialize, "db-initialize");
        startDaemon(this::keepAlive, "db-keep-alive");
    }

    private static void startDaemon(Runnable task, String name) {
        Thread t = new Thread(task, name);
        t.setDaemon(true);
        t.start();
    }

    public WorldRow executeQuery(int id) throws SQLException {
        DbConnection connection = connections.poll();
        if (connection == null) {
            connection = new DbConnection();
        }

        try {
            WorldRow result = connection.executeQuery(id);
            connections.add(connection);
            return result;
        } catch (SQLException e) {
            connection.close();
            throw e;
        }
    }

    private void initialize() {
        for (int i = 0; i < initialSize; i++) {
            try {
                connections.add(new DbConnection());
            } catch (SQLException e) {
                break;
            }
        }
    }

    private void keepAlive() {
        while (true) {
            DbConnection connection;
            while ((connection = connections.peek()) != null && connection.isIdle(maxIdle)) {
                rotateConnection(); // probabilistic - may rotate next connection in line, that's okay
            }
            try {
                Thread.sleep(1_000); // wait a moment for next idle check
            } catch (InterruptedException e) {
                break;
            }
        }
    }

    private void rotateConnection() {
        DbConnection connection = connections.poll(); // take from front of queue
        if (connection != null) {
            try {
                connection.executeQuery(1);
                connections.add(connection); // rotate to back of queue
            } catch (SQLException ignore) {
                connection.close();
            }
        }
    }

}
