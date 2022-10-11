package benchmark;

import io.vertx.core.AsyncResult;
import io.vertx.sqlclient.Pool;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.SqlClient;
import io.vertx.sqlclient.Tuple;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

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

    protected <T> Mono<T> executeAndCollectOne(String sql, Tuple tuple, Function<Row, T> mapper) {
        Sinks.One<T> sink = Sinks.one();
        client.preparedQuery(sql).execute(tuple, event -> {
            if (event.failed()) {
                sink.emitError(event.cause(), Sinks.EmitFailureHandler.FAIL_FAST);
            } else  {
                RowIterator<Row> iterator = event.result().iterator();
                if (iterator.hasNext()) {
                    sink.emitValue(mapper.apply(iterator.next()), Sinks.EmitFailureHandler.FAIL_FAST);
                } else {
                    sink.emitEmpty(Sinks.EmitFailureHandler.FAIL_FAST);
                }
            }
        });
        return sink.asMono();
    }

    protected <T> Mono<List<T>> executeAndCollectList(String sql, Function<Row, T> mapper) {
        Sinks.One<List<T>> sink = Sinks.one();
        client.preparedQuery(sql).execute(event -> {
            if (event.failed()) {
                sink.emitError(event.cause(), Sinks.EmitFailureHandler.FAIL_FAST);
            } else {
                List<T> list = new ArrayList<>();
                for (Row row : event.result()) {
                    list.add(mapper.apply(row));
                }
                sink.emitValue(list, Sinks.EmitFailureHandler.FAIL_FAST);
            }
        });
        return sink.asMono();
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
