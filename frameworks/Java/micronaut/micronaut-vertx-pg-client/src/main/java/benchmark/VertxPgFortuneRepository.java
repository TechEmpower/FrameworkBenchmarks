package benchmark;

import benchmark.model.Fortune;
import benchmark.repository.AsyncFortuneRepository;
import io.vertx.sqlclient.Pool;
import io.vertx.sqlclient.Tuple;
import jakarta.inject.Singleton;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletionStage;
import java.util.stream.Collectors;

@Singleton
public class VertxPgFortuneRepository extends AbstractVertxSqlClientRepository implements AsyncFortuneRepository {

    public VertxPgFortuneRepository(Pool client) {
        super(client);
    }

    private CompletionStage<?> createTable() {
        return execute("DROP TABLE IF EXISTS Fortune;")
                .thenCompose(ignore -> execute("CREATE TABLE Fortune (id INTEGER NOT NULL,message VARCHAR(255) NOT NULL);"));
    }

    @Override
    public CompletionStage<?> initDb(Collection<Fortune> fortunes) {
        List<Tuple> data = fortunes.stream().map(fortune -> Tuple.of(fortune.id(), fortune.message())).collect(Collectors.toList());
        return createTable().thenCompose(ignore -> executeBatch("INSERT INTO Fortune VALUES ($1, $2);", data));
    }

    @Override
    public CompletionStage<List<Fortune>> findAll() {
        return executeAndCollectList("SELECT * FROM Fortune", row -> new Fortune(row.getInteger(0), row.getString(1)));
    }

}
