package benchmark.repository;

import benchmark.model.Fortune;
import benchmark.model.World;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface DbRepository {
    Mono<World> getWorld(int id);

    Mono<World> findAndUpdateWorld(int id, int randomNumber);

    Flux<Fortune> fortunes();
}