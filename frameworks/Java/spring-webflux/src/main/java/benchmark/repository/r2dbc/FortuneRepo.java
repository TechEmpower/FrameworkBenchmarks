package benchmark.repository.r2dbc;

import benchmark.model.Fortune;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;

public interface FortuneRepo extends ReactiveCrudRepository<Fortune, Long> {

}
