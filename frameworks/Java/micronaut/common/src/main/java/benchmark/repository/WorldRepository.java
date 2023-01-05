package benchmark.repository;

import benchmark.model.World;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public interface WorldRepository {

    void initDb(Collection<World> worlds);

    World findById(Integer id);

    default List<World> findByIds(List<Integer> ids) {
        // Batch cannot be used by rules
        List<World> worlds = new ArrayList<>(ids.size());
        for (Integer number : ids) {
            worlds.add(findById(number));
        }
        return worlds;
    }

    void updateAll(Collection<World> worlds);

}
