package benchmark.repository;

import benchmark.model.Fortune;
import benchmark.model.World;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;

@Component
@Profile("jdbc")
public class JdbcDbRepository implements DbRepository {
    private final Logger log = LoggerFactory.getLogger(getClass());
    private final JdbcTemplate jdbcTemplate;
    private final Scheduler scheduler;

    public JdbcDbRepository(JdbcTemplate jdbcTemplate, Scheduler scheduler) {
        this.jdbcTemplate = jdbcTemplate;
        this.scheduler = scheduler;
    }

    @Override
    public Mono<World> getWorld(int id) {
        log.debug("getWorld({})", id);
        return Mono.fromCallable(() -> {
            return jdbcTemplate.queryForObject(
                    "SELECT * FROM world WHERE id = ?",
                    (rs, rn) -> new World(rs.getInt("id"), rs.getInt("randomnumber")),
                    id);
        }).subscribeOn(scheduler);
    }

    private Mono<World> updateWorld(World world) {
        return Mono.fromCallable(() -> {
            jdbcTemplate.update(
                    "UPDATE world SET randomnumber = ? WHERE id = ?",
                    world.randomnumber,
                    world.id);
            return world;
        }).subscribeOn(scheduler);
    }

    @Override
    public Mono<World> findAndUpdateWorld(int id, int randomNumber) {
        return getWorld(id).flatMap(world -> {
            world.randomnumber = randomNumber;
            return updateWorld(world);
        });
    }

    @Override
    public Flux<Fortune> fortunes() {
        return Mono.fromCallable(() -> {
            return jdbcTemplate.query(
                    "SELECT * FROM fortune",
                    (rs, rn) -> new Fortune(rs.getInt("id"), rs.getString("message")));
        }).subscribeOn(scheduler).flatMapIterable(fortunes -> fortunes);
    }
}