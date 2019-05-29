package benchmark.repository;

import benchmark.model.Fortune;
import benchmark.model.World;
import org.springframework.context.annotation.Profile;
import org.springframework.data.r2dbc.core.DatabaseClient;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Component
@Profile("r2dbc")
public class R2dbcDbRepository implements DbRepository {
    private final DatabaseClient databaseClient;

    public R2dbcDbRepository(DatabaseClient databaseClient) {
        this.databaseClient = databaseClient;
    }

    @Override
    public Mono<World> getWorld(int id) {
        return databaseClient.execute()
                .sql("SELECT id, randomnumber FROM world WHERE id = $1")
                .bind("$1", id)
                .as(World.class)
                .fetch()
                .first();

    }

    public Mono<World> updateWorld(World world) {
        return databaseClient.execute()
                .sql("UPDATE world SET randomnumber=$2 WHERE id = $1")
                .bind("$1", world.id)
                .bind("$2", world.randomnumber)
                .fetch()
                .rowsUpdated()
                .map(count -> world);
    }

    public Mono<World> findAndUpdateWorld(int id, int randomNumber) {
        return getWorld(id).flatMap(world -> {
            world.randomnumber = randomNumber;
            return updateWorld(world);
        });
    }

    @Override
    public Flux<Fortune> fortunes() {
        return databaseClient.execute()
                .sql("SELECT id, message FROM fortune")
                .as(Fortune.class)
                .fetch()
                .all();
    }
}