package benchmark;

import benchmark.model.World;
import benchmark.repository.ReactiveWorldRepository;
import io.vertx.core.Future;
import io.vertx.sqlclient.Pool;
import io.vertx.sqlclient.Tuple;
import jakarta.inject.Singleton;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Singleton
public class VertxPgWorldRepository extends AbstractVertxSqlClientRepository implements ReactiveWorldRepository {

    public VertxPgWorldRepository(Pool client) {
        super(client);
    }

    private Mono<Void> createTable() {
        return execute("DROP TABLE IF EXISTS World;").then(execute("CREATE TABLE World (id INTEGER NOT NULL,randomNumber INTEGER NOT NULL);").then());
    }

    @Override
    public Mono<Void> initDb(Collection<World> worlds) {
        List<Tuple> data = worlds.stream().map(world -> Tuple.of(world.getId(), world.getRandomNumber())).collect(Collectors.toList());
        return createTable().then(executeBatch("INSERT INTO world VALUES ($1, $2);", data).then());
    }

    @Override
    public Publisher<World> findById(Integer id) {
        return execute("SELECT * FROM world WHERE id = $1", Tuple.of(id))
                .map(row -> new World(row.getInteger(0), row.getInteger(1)));
    }

    @Override
    public Publisher<List<World>> findByIds(List<Integer> ids) {
        return asMono(
                client.withConnection(sqlConnection -> asFuture(
                        Flux.fromIterable(ids)
                                .flatMap(id -> execute(sqlConnection, "SELECT * FROM world WHERE id = $1", Tuple.of(id))
                                        .map(row -> new World(row.getInteger(0), row.getInteger(1))))
                                .collectList()))
        );
    }

    private <T> Mono<T> asMono(Future<T> future) {
        return Mono.fromFuture(future.toCompletionStage().toCompletableFuture());
    }

    private <T> Future<T> asFuture(Mono<T> mono) {
        return Future.fromCompletionStage(mono.toFuture());
    }

    @Override
    public Publisher<Void> updateAll(Collection<World> worlds) {
        List<Tuple> data = worlds.stream().map(world -> Tuple.of(world.getId(), world.getRandomNumber())).collect(Collectors.toList());
        return executeBatch("UPDATE world SET randomnumber = $2 WHERE id = $1", data).then();
    }

}
