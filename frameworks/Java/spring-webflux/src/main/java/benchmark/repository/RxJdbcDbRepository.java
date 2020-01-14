package benchmark.repository;

import benchmark.model.Fortune;
import benchmark.model.World;
import io.reactivex.Flowable;
import org.davidmoten.rx.jdbc.Database;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Component
@Profile("rxjdbc")
public class RxJdbcDbRepository implements DbRepository {
    private final Database db;

    public RxJdbcDbRepository(Database db) {
        this.db = db;
    }

    @Override
    public Mono<World> getWorld(int id) {
        String sql = "SELECT * FROM world WHERE id = ?";

        Flowable<World> worldFlowable = db.select(sql)
                .parameters(id)
                .get(rs -> {
                    World world = new World(rs.getInt("id"), rs.getInt("randomnumber"));
                    return world;
                });

        return Mono.from(worldFlowable);
    }

    public Mono<World> updateWorld(World world) {
        String sql = "UPDATE world SET randomnumber = ? WHERE id = ?";

        Flowable<World> worldFlowable = db.update(sql)
                .parameters(world.randomnumber, world.id)
                .counts().map(cnt -> world);
        return Mono.from(worldFlowable);
    }

    public Mono<World> findAndUpdateWorld(int id, int randomNumber) {
        return getWorld(id).flatMap(world -> {
            world.randomnumber = randomNumber;
            return updateWorld(world);
        });
    }

    @Override
    public Flux<Fortune> fortunes() {
        String sql = "SELECT * FROM fortune";

        Flowable<Fortune> fortuneFlowable = db.select(sql)
                .get(rs -> new Fortune(rs.getInt("id"), rs.getString("message")));

        return Flux.from(fortuneFlowable);
    }
}