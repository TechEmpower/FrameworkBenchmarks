package benchmark;

import benchmark.model.World;
import benchmark.repository.ReactiveWorldRepository;
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
public class R2dbcWorldRepository implements ReactiveWorldRepository {

    private final ConnectionFactory connectionFactory;

    public R2dbcWorldRepository(ConnectionFactory connectionFactory) {
        this.connectionFactory = connectionFactory;
    }

    @Override
    public Publisher<Void> initDb(Collection<World> worlds) {
        return Mono.from(connectionFactory.create())
                .flatMap(connection -> createTable(connection).then(createWorlds(connection, worlds)).then(Mono.from(connection.close())));
    }

    private static Mono<Void> createTable(io.r2dbc.spi.Connection connection) {
        return execute(connection, "DROP TABLE IF EXISTS World;")
                .then(execute(connection, "CREATE TABLE World (id INTEGER NOT NULL,randomNumber INTEGER NOT NULL);"));
    }

    private static Mono<Void> execute(io.r2dbc.spi.Connection connection, String sql) {
        return Flux.from(connection.createStatement(sql).execute())
                .flatMap(Result::getRowsUpdated).then();
    }

    private static Mono<Void> createWorlds(io.r2dbc.spi.Connection connection, Collection<World> worlds) {
        Statement statement = connection.createStatement("INSERT INTO world VALUES ($1, $2);");
        boolean first = true;
        for (World world : worlds) {
            if (!first) {
                statement.add();
            } else {
                first = false;
            }
            statement.bind(0, world.getId());
            statement.bind(1, world.getRandomNumber());
        }
        return Flux.from(statement.execute()).flatMap(Result::getRowsUpdated).then();
    }

    @Override
    public Publisher<World> findById(Integer id) {
        return Mono.usingWhen(connectionFactory.create(),
                connection -> Mono.from(connection.createStatement("SELECT * FROM world WHERE id = $1").bind(0, id).execute())
                        .flatMap(result -> Mono.from(result.map((row, rowMetadata) -> new World(row.get(0, Integer.class), row.get(1, Integer.class))))),
                Connection::close);
    }

    @Override
    public Publisher<List<World>> findByIds(List<Integer> ids) {
        return Mono.usingWhen(connectionFactory.create(),
                connection -> {
                    Statement statement = connection.createStatement("SELECT * FROM world WHERE id = $1");
                    boolean first = true;
                    for (Integer id : ids) {
                        if (!first) {
                            statement.add();
                        } else {
                            first = false;
                        }
                        statement.bind(0, id);
                    }
                    return Flux.from(statement.execute())
                            .flatMap(result -> Flux.from(result.map((row, rowMetadata) -> new World(row.get(0, Integer.class), row.get(1, Integer.class)))))
                            .collectList();
                },
                Connection::close);
    }

    @Override
    public Publisher<Void> updateAll(Collection<World> worlds) {
        return Mono.usingWhen(connectionFactory.create(),
                connection -> {
                    Statement statement = connection.createStatement("UPDATE world SET randomnumber = $2 WHERE id = $1");
                    boolean first = true;
                    for (World world : worlds) {
                        if (!first) {
                            statement.add();
                        } else {
                            first = false;
                        }
                        statement.bind(0, world.getId());
                        statement.bind(1, world.getRandomNumber());
                    }
                    return Flux.from(statement.execute()).flatMap(Result::getRowsUpdated).then();
                },
                Connection::close).then();
    }

}
