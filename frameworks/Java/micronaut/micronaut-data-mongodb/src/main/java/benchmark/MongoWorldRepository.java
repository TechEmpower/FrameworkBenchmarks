package benchmark;

import benchmark.model.World;
import benchmark.repository.WorldRepository;
import io.micronaut.data.mongodb.annotation.MongoRepository;
import io.micronaut.data.repository.GenericRepository;
import io.micronaut.transaction.annotation.ReadOnly;

import java.util.Collection;
import java.util.List;

@MongoRepository
public interface MongoWorldRepository extends GenericRepository<World, Integer>, WorldRepository {

    default void initDb(Collection<World> worlds) {
        deleteAll();
        saveAll(worlds);
    }

    void saveAll(Collection<World> worlds);

    void deleteAll();

    @ReadOnly
    @Override
    default List<World> findByIds(List<Integer> ids) {
        return WorldRepository.super.findByIds(ids);
    }
}
