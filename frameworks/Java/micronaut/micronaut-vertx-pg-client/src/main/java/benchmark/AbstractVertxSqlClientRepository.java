package benchmark;

import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.sqlclient.Pool;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.SqlClient;
import io.vertx.sqlclient.Tuple;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletionStage;
import java.util.function.Function;

public class AbstractVertxSqlClientRepository {

    protected final Pool client;

    public AbstractVertxSqlClientRepository(Pool client) {
        this.client = client;
    }

    protected CompletionStage<?> execute(String sql) {
        return client.preparedQuery(sql).execute().toCompletionStage();
    }

    protected <T> CompletionStage<T> executeAndCollectOne(String sql, Tuple tuple, Function<Row, T> mapper) {
        return client.preparedQuery(sql).execute(tuple).map(rows -> mapper.apply(rows.iterator().next()))
                .toCompletionStage();
    }

    protected <T> CompletionStage<List<T>> executeAndCollectList(String sql, Function<Row, T> mapper) {
        return client.preparedQuery(sql).execute().map(rows -> {
            List<T> result = new ArrayList<>(rows.size());
            for (Row row : rows) {
                result.add(mapper.apply(row));
            }
            return result;
        }).toCompletionStage();
    }

    protected <T> Future<List<T>> executeMany(SqlClient sqlClient, String sql, List<Tuple> data, Function<Row, T> mapper) {
        List<Future<T>> futures = new ArrayList<>(data.size());
        Function<RowSet<Row>, T> rowsMapper = rows -> mapper.apply(rows.iterator().next());
        for (Tuple d : data) {
            futures.add(
                    sqlClient.preparedQuery(sql).execute(d).map(rowsMapper)
            );
        }
        return Future.all(futures).map(CompositeFuture::list);
    }

    protected CompletionStage<?> executeBatch(String sql, List<Tuple> data) {
        return client.preparedQuery(sql).executeBatch(data).toCompletionStage();
    }

}
