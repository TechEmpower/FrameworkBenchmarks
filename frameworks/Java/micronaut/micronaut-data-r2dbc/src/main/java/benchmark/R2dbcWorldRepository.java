package benchmark;

import benchmark.model.World;
import benchmark.repository.ReactiveWorldRepository;
import io.micronaut.data.model.query.builder.sql.Dialect;
import io.micronaut.data.r2dbc.annotation.R2dbcRepository;
import io.micronaut.data.repository.GenericRepository;
import io.micronaut.transaction.annotation.ReadOnly;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Mono;

import javax.transaction.Transactional;
import java.util.Collection;
import java.util.List;

@R2dbcRepository(dialect = Dialect.POSTGRES)
public interface R2dbcWorldRepository extends GenericRepository<World, Integer>, ReactiveWorldRepository {

    default Mono<Void> initDb(Collection<World> worlds) {
        return deleteAll().then(saveAll(worlds));
    }

    @Override
    default Publisher<List<World>> findByIds(List<Integer> ids) {
        return ReactiveWorldRepository.super.findByIds(ids);
    }

    Mono<Void> saveAll(Collection<World> worlds);

    Mono<Void> deleteAll();

}
