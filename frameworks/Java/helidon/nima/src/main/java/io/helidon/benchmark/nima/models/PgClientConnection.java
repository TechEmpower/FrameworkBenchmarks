
package io.helidon.benchmark.nima.models;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import io.vertx.pgclient.PgConnection;
import io.vertx.sqlclient.PreparedQuery;
import io.vertx.sqlclient.PreparedStatement;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;

public class PgClientConnection implements AutoCloseable {
    static final int UPDATE_QUERIES = 500;
    private static String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    private static String SELECT_FORTUNE = "SELECT * from FORTUNE";

    private CompletableFuture<PreparedStatement> worldQuery;
    private CompletableFuture<PreparedStatement> fortuneQuery;
    private CompletableFuture<PreparedStatement>[] updateQuery;

    private final PgConnection conn;

    PgClientConnection(PgConnection conn) {
        this.conn = conn;
    }

    public PgConnection pgConnection() {
        return conn;
    }

    @Override
    public void close() {
        conn.close();
    }

    public PreparedQuery<RowSet<Row>> worldQuery() throws ExecutionException, InterruptedException {
        return worldQuery.get().query();
    }

    public PreparedQuery<RowSet<Row>> fortuneQuery() throws ExecutionException, InterruptedException {
        return fortuneQuery.get().query();
    }

    public PreparedQuery<RowSet<Row>> updateQuery(int queryCount) throws ExecutionException, InterruptedException {
        return updateQuery[queryCount - 1].get().query();
    }

    @SuppressWarnings("unchecked")
    void prepare() {
        try {
            worldQuery = conn.prepare(SELECT_WORLD).toCompletionStage().toCompletableFuture();
            fortuneQuery = conn.prepare(SELECT_FORTUNE).toCompletionStage().toCompletableFuture();
            updateQuery = (CompletableFuture<PreparedStatement>[]) new CompletableFuture<?>[UPDATE_QUERIES];
            for (int i = 0; i < UPDATE_QUERIES; i++) {
                updateQuery[i] = conn.prepare(singleUpdate(i + 1))
                                     .toCompletionStage().toCompletableFuture();
                    }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static String singleUpdate(int count) {
        StringBuilder sql = new StringBuilder();
        sql.append("UPDATE WORLD SET RANDOMNUMBER = CASE ID");
        for (int i = 0; i < count; i++) {
            int k = i * 2 + 1;
            sql.append(" WHEN $").append(k).append(" THEN $").append(k + 1);
        }
        sql.append(" ELSE RANDOMNUMBER");
        sql.append(" END WHERE ID IN ($1");
        for (int i = 1; i < count; i++) {
            int k = i * 2 + 1;
            sql.append(",$").append(k);
        }
        sql.append(")");
        return sql.toString();
    }
}
