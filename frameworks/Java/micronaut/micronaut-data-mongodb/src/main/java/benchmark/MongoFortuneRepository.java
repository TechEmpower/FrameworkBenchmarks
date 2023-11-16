package benchmark;

import benchmark.model.Fortune;
import benchmark.repository.FortuneRepository;
import io.micronaut.data.mongodb.annotation.MongoRepository;
import io.micronaut.data.repository.GenericRepository;

import java.util.Collection;

@MongoRepository
public interface MongoFortuneRepository extends GenericRepository<Fortune, Integer>, FortuneRepository {

    default void initDb(Collection<Fortune> fortunes) {
        deleteAll();
        saveAll(fortunes);
    }

    void saveAll(Collection<Fortune> fortunes);

    void deleteAll();

}
