package benchmark;

import benchmark.model.Fortune;
import benchmark.repository.FortuneRepository;
import io.micronaut.data.jdbc.annotation.JdbcRepository;
import io.micronaut.data.model.query.builder.sql.Dialect;
import io.micronaut.data.repository.GenericRepository;

import java.util.Collection;

@JdbcRepository(dialect = Dialect.POSTGRES)
public interface JdbcFortuneRepository extends GenericRepository<Fortune, Integer>, FortuneRepository {

    default void initDb(Collection<Fortune> fortunes) {
        deleteAll();
        saveAll(fortunes);
    }

    void saveAll(Collection<Fortune> fortunes);

    void deleteAll();

}
