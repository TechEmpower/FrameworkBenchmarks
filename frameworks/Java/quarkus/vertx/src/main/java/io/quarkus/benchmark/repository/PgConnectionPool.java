package io.quarkus.benchmark.repository;

import io.netty.util.concurrent.EventExecutor;
import io.netty.util.concurrent.FastThreadLocal;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgConnection;
import io.vertx.sqlclient.PreparedQuery;
import io.vertx.sqlclient.PreparedStatement;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.impl.SqlClientInternal;

import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReferenceArray;

public class PgConnectionPool implements AutoCloseable {

    static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    static final String SELECT_FORTUNE = "SELECT id, message from FORTUNE";
    private final FastThreadLocal<PgClientConnection> pgConnectionPool;
    private final AtomicReferenceArray<PgClientConnection> pgConnections;

    public PgConnectionPool(final Vertx vertx, final PgConnectOptions options) {
        final var executors = new ArrayList<EventExecutor>(Runtime.getRuntime().availableProcessors());
        vertx.nettyEventLoopGroup().forEach(executors::add);
        final var connectionsCompleted = new CompletableFuture<>();
        final var completedConnections = new AtomicReferenceArray<AsyncResult<PgClientConnection>>(executors.size());
        final var allCompleted = new AtomicInteger(executors.size());
        final var connectionAffinityMap = new ConcurrentHashMap<Thread, PgClientConnection>(executors.size());
        for (int i = 0; i < executors.size(); i++) {
            final int executorId = i;
            executors.get(i).execute(() -> connect(vertx, options)
                    .onComplete(ar -> {
                        final boolean lastCompleted = allCompleted.decrementAndGet() == 0;
                        if (!completedConnections.compareAndSet(executorId, null, ar)) {
                            if (ar.succeeded()) {
                                ar.result().connection.close();
                            }
                        } else if (ar.succeeded()) {
                            // assign the executorId to the connection
                            ar.result().executorId = executorId;
                            connectionAffinityMap.put(Thread.currentThread(), ar.result());
                        }
                        if (lastCompleted) {
                            connectionsCompleted.complete(null);
                        }
                    }));
        }
        // TODO make the global timeout to be configurable
        try {
            connectionsCompleted.join();
        } catch (final Throwable t) {
            // let's forcibly close all completed connections
            forceCloseEstablishedConnections(completedConnections);
            throw new IllegalStateException("cannot establish all connections", t);
        }
        // let's fast-fail if we cannot establish all connections
        pgConnections = new AtomicReferenceArray<>(completedConnections.length());
        for (int i = 0; i < completedConnections.length(); i++) {
            final AsyncResult<PgClientConnection> ar = completedConnections.get(i);
            if (ar == null || ar.failed()) {
                forceCloseEstablishedConnections(completedConnections);
                throw new IllegalStateException("cannot establish all connections");
            } else {
                pgConnections.set(i, ar.result());
            }
        }
        pgConnectionPool = new FastThreadLocal<>() {
            @Override
            protected PgClientConnection initialValue() {
                return connectionAffinityMap.get(Thread.currentThread());
            }

            @Override
            protected void onRemoval(final PgClientConnection value) {
                final PgClientConnection removed = connectionAffinityMap.remove(Thread.currentThread());
                if (removed != null) {
                    final var connectionToClose = pgConnections.getAndSet(removed.executorId, null);
                    if (connectionToClose != null) {
                        assert connectionToClose == removed;
                        connectionToClose.connection.close();
                    }
                }
            }
        };
    }

    private static <T> Handler<AsyncResult<T>> onSuccess(final Handler<T> handler) {
        return ar -> {
            if (ar.succeeded()) {
                handler.handle(ar.result());
            }
        };
    }

    private static Future<PgClientConnection> connect(final Vertx vertx, final PgConnectOptions options) {
        final PgClientConnection result = new PgClientConnection();
        final Promise<PgClientConnection> connectionEstablished = Promise.promise();
        final var future = PgConnection.connect(vertx, options)
                .flatMap(conn -> {
                    result.connection = (SqlClientInternal) conn;
                    final Future<PreparedStatement> f1 = conn.prepare(SELECT_WORLD)
                            .andThen(onSuccess(ps -> result.SELECT_WORLD_QUERY = ps.query()));
                    final Future<PreparedStatement> f2 = conn.prepare(SELECT_FORTUNE)
                            .andThen(onSuccess(ps -> result.SELECT_FORTUNE_QUERY = ps.query()));

                    final int QUERIES = 500;
                    result.UPDATE_WORLD_QUERY = new PreparedQuery[QUERIES];
                    var updateWorldQueries = new ArrayList<Future<PreparedStatement>>(QUERIES);
                    for (int queryCount = 1; queryCount <= QUERIES; queryCount++) {
                        updateWorldQueries.add(result.prepareUpdateQueryFor(conn, queryCount));
                    }
                    return Future.join(f1, f2, Future.join(updateWorldQueries));
                }).onComplete(ar -> {
                    if (ar.failed() && result.connection != null) {
                        result.connection.close();
                    }
                });
        future.onComplete(ar -> {
            if (ar.succeeded()) {
                connectionEstablished.complete(result);
            } else {
                connectionEstablished.fail(ar.cause());
            }
        });
        return connectionEstablished.future();
    }

    private static void forceCloseEstablishedConnections(final AtomicReferenceArray<AsyncResult<PgClientConnection>> completedConnections) {
        final AsyncResult<PgClientConnection> noResult = Future.succeededFuture();
        for (int i = 0; i < completedConnections.length(); i++) {
            final AsyncResult<PgClientConnection> ar = completedConnections.getAndSet(i, noResult);
            if (ar != null && ar.succeeded() && ar.result() != null && ar.result().connection != null) {
                try {
                    ar.result().connection.close();
                } catch (final Throwable t2) {
                    // ignore
                }
            }
        }
    }

    public PgClientConnection pgConnection() {
        return pgConnectionPool.get();
    }

    @Override
    public void close() {
        for (int i = 0; i < pgConnections.length(); i++) {
            final var connection = pgConnections.getAndSet(i, null);
            if (connection != null) {
                connection.connection.close();
            }
        }
    }

    public static class PgClientConnection {
        private int executorId;
        private PreparedQuery<RowSet<Row>> SELECT_WORLD_QUERY;
        private PreparedQuery<RowSet<Row>> SELECT_FORTUNE_QUERY;
        private PreparedQuery<RowSet<Row>>[] UPDATE_WORLD_QUERY;
        private SqlClientInternal connection;

        public PreparedQuery<RowSet<Row>> selectWorldQuery() {
            return SELECT_WORLD_QUERY;
        }

        public PreparedQuery<RowSet<Row>> selectFortuneQuery() {
            return SELECT_FORTUNE_QUERY;
        }

        public PreparedQuery<RowSet<Row>> updateWorldQuery(int queryCount) {
            return UPDATE_WORLD_QUERY[queryCount - 1];
        }

        private Future<PreparedStatement> prepareUpdateQueryFor(PgConnection connection, int queryCount) {
            return connection.prepare(updateQueryFor(queryCount)).andThen(onSuccess(ps -> UPDATE_WORLD_QUERY[queryCount - 1] = ps.query()));
        }
        private static String updateQueryFor(int queryCount) {
            StringBuilder sql = new StringBuilder();
            sql.append("UPDATE WORLD SET RANDOMNUMBER = CASE ID");
            for (int i = 0; i < queryCount; i++) {
                int offset = (i * 2) + 1;
                sql.append(" WHEN $" + offset + " THEN $" + (offset + 1));
            }
            sql.append(" ELSE RANDOMNUMBER");
            sql.append(" END WHERE ID IN ($1");
            for (int i = 1; i < queryCount; i++) {
                int offset = (i * 2) + 1;
                sql.append(",$" + offset);
            }
            sql.append(")");
            return sql.toString();
        }

        public SqlClientInternal rawConnection() {
            return connection;
        }
    }
}