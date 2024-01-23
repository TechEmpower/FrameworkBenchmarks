package benchmark;

import benchmark.model.World;
import benchmark.repository.AsyncWorldRepository;
import io.vertx.sqlclient.Pool;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.Tuple;
import jakarta.inject.Singleton;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletionStage;
import java.util.function.Function;
import java.util.stream.Collectors;

@Singleton
public class VertxPgWorldRepository extends AbstractVertxSqlClientRepository implements AsyncWorldRepository {

    private static final Function<Row, World> ROW_TO_WORLD_MAPPER = row -> new World(row.getInteger(0), row.getInteger(1));

    public VertxPgWorldRepository(Pool client) {
        super(client);
    }

    private CompletionStage<?> createTable() {
        return execute("DROP TABLE IF EXISTS World;").thenCompose(ignore -> execute("CREATE TABLE World (id INTEGER NOT NULL,randomNumber INTEGER NOT NULL);"));
    }

    @Override
    public CompletionStage<?> initDb(Collection<World> worlds) {
        List<Tuple> data = worlds.stream().map(world -> Tuple.of(world.getId(), world.getRandomNumber())).collect(Collectors.toList());
        return createTable().thenCompose(ignore -> executeBatch("INSERT INTO world VALUES ($1, $2);", data));
    }

    @Override
    public CompletionStage<World> findById(Integer id) {
        return executeAndCollectOne("SELECT * FROM world WHERE id = $1", Tuple.of(id), ROW_TO_WORLD_MAPPER);
    }

    @Override
    public CompletionStage<List<World>> findByIds(List<Integer> ids) {
        List<Tuple> data = new ArrayList<>(ids.size());
        for (Integer id : ids) {
            data.add(Tuple.of(id));
        }
        return client.withConnection(sqlConnection ->
                executeMany(sqlConnection, "SELECT * FROM world WHERE id = $1", data, ROW_TO_WORLD_MAPPER))
                .toCompletionStage();
    }

    @Override
    public CompletionStage<?> updateAll(Collection<World> worlds) {
        List<Tuple> data = new ArrayList<>(worlds.size());
        for (World world : worlds) {
            data.add(Tuple.of(world.getId(), world.getRandomNumber()));
        }
        return executeBatch("UPDATE world SET randomnumber = $2 WHERE id = $1", data);
    }

}
