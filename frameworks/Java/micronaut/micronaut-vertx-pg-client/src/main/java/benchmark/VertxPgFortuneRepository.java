package benchmark;

import benchmark.model.Fortune;
import benchmark.repository.ReactiveFortuneRepository;
import io.vertx.sqlclient.Pool;
import io.vertx.sqlclient.Tuple;
import jakarta.inject.Singleton;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Singleton
public class VertxPgFortuneRepository extends AbstractVertxSqlClientRepository implements ReactiveFortuneRepository {

    public VertxPgFortuneRepository(Pool client) {
        super(client);
    }

    private Mono<Void> createTable() {
        return execute("DROP TABLE IF EXISTS Fortune;").then(execute("CREATE TABLE Fortune (id INTEGER NOT NULL,message VARCHAR(255) NOT NULL);").then());
    }

    @Override
    public Publisher<Void> initDb(Collection<Fortune> fortunes) {
        List<Tuple> data = fortunes.stream().map(fortune -> Tuple.of(fortune.getId(), fortune.getMessage())).collect(Collectors.toList());
        return createTable().then(executeBatch("INSERT INTO Fortune VALUES ($1, $2);", data).then());
    }

    @Override
    public Publisher<List<Fortune>> findAll() {
        return executeAndCollectList("SELECT * FROM Fortune", row -> new Fortune(row.getInteger(0), row.getString(1)));
    }

}
