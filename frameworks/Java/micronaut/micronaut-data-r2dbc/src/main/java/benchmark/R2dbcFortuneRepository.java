package benchmark;

import benchmark.model.Fortune;
import benchmark.repository.ReactiveFortuneRepository;
import io.micronaut.data.model.query.builder.sql.Dialect;
import io.micronaut.data.r2dbc.annotation.R2dbcRepository;
import io.micronaut.data.repository.GenericRepository;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.List;

@R2dbcRepository(dialect = Dialect.POSTGRES)
public interface R2dbcFortuneRepository extends GenericRepository<Fortune, Integer>, ReactiveFortuneRepository {

    default Publisher<Void> initDb(Collection<Fortune> fortunes) {
        return deleteAll().then(saveAll(fortunes));
    }

    Mono<Void> saveAll(Collection<Fortune> fortunes);

    Mono<Void> deleteAll();

    @Override
    default Publisher<List<Fortune>> findAll() {
        return Flux.from(queryAll()).collectList();
    }

    Publisher<Fortune> queryAll();
}
