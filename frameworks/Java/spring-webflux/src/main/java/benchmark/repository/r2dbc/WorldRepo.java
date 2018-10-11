package benchmark.repository.r2dbc;

import benchmark.model.World;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;

public interface WorldRepo extends ReactiveCrudRepository<World, Integer> {
}
