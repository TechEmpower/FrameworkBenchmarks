package benchmark.repository;

import java.util.List;

import benchmark.model.Fortune;
import benchmark.model.World;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface DbRepository {

    Mono<World> getWorld(int id);

    Mono<Void> updateWorlds(List<World> worlds);

    Flux<Fortune> fortunes();
}