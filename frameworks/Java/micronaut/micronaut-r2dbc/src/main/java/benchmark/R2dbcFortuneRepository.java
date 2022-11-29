package benchmark;

import benchmark.model.Fortune;
import benchmark.repository.ReactiveFortuneRepository;
import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.Result;
import io.r2dbc.spi.Statement;
import jakarta.inject.Singleton;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.List;

@Singleton
public class R2dbcFortuneRepository implements ReactiveFortuneRepository {

    private final ConnectionFactory connectionFactory;

    public R2dbcFortuneRepository(ConnectionFactory connectionFactory) {
        this.connectionFactory = connectionFactory;
    }

    @Override
    public Publisher<Void> initDb(Collection<Fortune> fortunes) {
        return Mono.from(connectionFactory.create())
                .flatMap(connection -> createTable(connection).then(createFutures(connection, fortunes)).then(Mono.from(connection.close())));
    }

    private static Mono<Void> createTable(io.r2dbc.spi.Connection connection) {
        return execute(connection, "DROP TABLE IF EXISTS Fortune;")
                .then(execute(connection, "CREATE TABLE Fortune (id INTEGER NOT NULL,message VARCHAR(255));"));
    }

    private static Mono<Void> execute(io.r2dbc.spi.Connection connection, String sql) {
        return Flux.from(connection.createStatement(sql).execute())
                .flatMap(Result::getRowsUpdated).then();
    }

    private static Mono<Void> createFutures(io.r2dbc.spi.Connection connection, Collection<Fortune> fortunes) {
        Statement statement = connection.createStatement("INSERT INTO fortune VALUES ($1, $2);");
        boolean first = true;
        for (Fortune fortune : fortunes) {
            if (!first) {
                statement.add();
            } else {
                first = false;
            }
            statement.bind(0, fortune.getId());
            statement.bind(1, fortune.getMessage());
        }
        return Flux.from(statement.execute()).flatMap(Result::getRowsUpdated).then();
    }

    @Override
    public Publisher<List<Fortune>> findAll() {
        return Flux.usingWhen(connectionFactory.create(),
                connection -> Flux.from(connection.createStatement("SELECT id, message FROM fortune").execute())
                        .flatMap(result -> result.map((row, rowMetadata) -> new Fortune(row.get(0, Integer.class), row.get(1, String.class))))
                        .collectList(),
                Connection::close);
    }

}
