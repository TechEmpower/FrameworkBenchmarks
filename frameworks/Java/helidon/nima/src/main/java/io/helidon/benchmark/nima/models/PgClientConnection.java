
package io.helidon.benchmark.nima.models;

import io.vertx.pgclient.PgConnection;
import io.vertx.sqlclient.PreparedQuery;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;

public class PgClientConnection implements AutoCloseable {
    static final int UPDATE_QUERIES = 500;
    private static String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    private static String SELECT_FORTUNE = "SELECT * from FORTUNE";

    private PreparedQuery<RowSet<Row>> worldQuery;
    private PreparedQuery<RowSet<Row>> fortuneQuery;
    private PreparedQuery<RowSet<Row>>[] updateQuery;

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

    public PreparedQuery<RowSet<Row>> worldQuery() {
        return worldQuery;
    }

    public PreparedQuery<RowSet<Row>> fortuneQuery() {
        return fortuneQuery;
    }

    public PreparedQuery<RowSet<Row>> updateQuery(int queryCount) {
        return updateQuery[queryCount - 1];
    }

    @SuppressWarnings("unchecked")
    void prepare() {
        try {
            worldQuery = conn.prepare(SELECT_WORLD)
                    .toCompletionStage().toCompletableFuture().get().query();
            fortuneQuery = conn.prepare(SELECT_FORTUNE)
                    .toCompletionStage().toCompletableFuture().get().query();
            updateQuery = (PreparedQuery<RowSet<Row>>[]) new PreparedQuery<?>[UPDATE_QUERIES];
            for (int i = 0; i < UPDATE_QUERIES; i++) {
                updateQuery[i] = conn.prepare(singleUpdate(i + 1))
                        .toCompletionStage().toCompletableFuture().get().query();
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
