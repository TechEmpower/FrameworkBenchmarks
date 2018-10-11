package benchmark.repository;

import benchmark.model.Fortune;
import benchmark.model.World;
import benchmark.repository.r2dbc.FortuneRepo;
import benchmark.repository.r2dbc.WorldRepo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Component
@Profile("r2dbc")
public class R2dbcDbRepository implements DbRepository {
    private final Logger log = LoggerFactory.getLogger(getClass());
    private final WorldRepo worldRepo;
    private final FortuneRepo fortuneRepo;

    public R2dbcDbRepository(WorldRepo worldRepo, FortuneRepo fortuneRepo) {
        this.worldRepo = worldRepo;
        this.fortuneRepo = fortuneRepo;
    }

    @Override
    public Mono<World> getWorld(int id) {
        return worldRepo.findById(id)
                .doOnError(e -> log.error("Failed to get world with id {}", id, e));
    }

    public Mono<World> updateWorld(World world) {
        return worldRepo.save(world)
                .doOnError(e -> log.error("Failed to update world {}", world, e));
    }

    public Mono<World> findAndUpdateWorld(int id, int randomNumber) {
        return getWorld(id).flatMap(world -> {
            world.randomnumber = randomNumber;
            return updateWorld(world);
        });
    }

    @Override
    public Flux<Fortune> fortunes() {
        return fortuneRepo.findAll()
                .doOnError(e -> log.error("Failed to get fortunes", e));
    }
}
