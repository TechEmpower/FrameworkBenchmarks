package benchmark;

import benchmark.model.World;
import benchmark.repository.WorldRepository;
import io.micronaut.data.connection.annotation.Connectable;
import io.micronaut.data.jdbc.annotation.JdbcRepository;
import io.micronaut.data.model.query.builder.sql.Dialect;
import io.micronaut.data.repository.GenericRepository;
import io.micronaut.transaction.annotation.ReadOnly;

import java.util.Collection;
import java.util.List;

@JdbcRepository(dialect = Dialect.POSTGRES)
public interface JdbcWorldRepository extends GenericRepository<World, Integer>, WorldRepository {

    default void initDb(Collection<World> worlds) {
        deleteAll();
        saveAll(worlds);
    }

    void saveAll(Collection<World> worlds);

    @Connectable
    @Override
    default List<World> findByIds(List<Integer> ids) {
        return WorldRepository.super.findByIds(ids);
    }

    void deleteAll();

}
