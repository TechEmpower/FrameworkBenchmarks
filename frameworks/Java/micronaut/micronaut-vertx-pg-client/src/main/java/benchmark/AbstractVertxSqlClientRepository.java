package benchmark;

import io.vertx.core.AsyncResult;
import io.vertx.sqlclient.Pool;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.SqlClient;
import io.vertx.sqlclient.Tuple;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Sinks;

import java.util.List;

public class AbstractVertxSqlClientRepository {

    protected final Pool client;

    public AbstractVertxSqlClientRepository(Pool client) {
        this.client = client;
    }

    protected Flux<Row> execute(String sql) {
        return Flux.defer(() -> {
            Sinks.Many<Row> sink = Sinks.many().multicast().onBackpressureBuffer();
            client.preparedQuery(sql).execute(event -> mapResult(sink, event));
            return sink.asFlux();
        });
    }

    protected Flux<Row> execute(String sql, Tuple data) {
        return execute(client, sql, data);
    }

    protected Flux<Row> execute(SqlClient sqlClient, String sql, Tuple data) {
        return Flux.defer(() -> {
            Sinks.Many<Row> sink = Sinks.many().multicast().onBackpressureBuffer();
            sqlClient.preparedQuery(sql).execute(data, event -> mapResult(sink, event));
            return sink.asFlux();
        });
    }

    protected Flux<Row> executeBatch(String sql, List<Tuple> data) {
        return Flux.defer(() -> {
            Sinks.Many<Row> sink = Sinks.many().multicast().onBackpressureBuffer();
            client.preparedQuery(sql).executeBatch(data, event -> mapResult(sink, event));
            return sink.asFlux();
        });
    }

    private void mapResult(Sinks.Many<Row> sink, AsyncResult<RowSet<Row>> event) {
        if (event.failed()) {
            sink.emitError(event.cause(), Sinks.EmitFailureHandler.FAIL_FAST);
        } else {
            for (Row row : event.result()) {
                sink.emitNext(row, Sinks.EmitFailureHandler.FAIL_FAST);
            }
            sink.emitComplete(Sinks.EmitFailureHandler.FAIL_FAST);
        }
    }

}
