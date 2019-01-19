package benchmark.repository;

import benchmark.PgClients;
import benchmark.model.Fortune;
import benchmark.model.World;
import io.reactiverse.pgclient.PgIterator;
import io.reactiverse.pgclient.Row;
import io.reactiverse.pgclient.Tuple;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Component
@Profile("pgclient")
public class PgClientDbRepository implements DbRepository {
    private final Logger log = LoggerFactory.getLogger(getClass());
    private final PgClients pgClients;

    public PgClientDbRepository(PgClients pgClients) {
        this.pgClients = pgClients;
    }

    @Override
    public Mono<World> getWorld(int id) {
        return Mono.create(sink ->
                pgClients.getOne().preparedQuery("SELECT * FROM world WHERE id = $1", Tuple.of(id), ar -> {
                    if (ar.failed()) {
                        sink.error(ar.cause());
                    } else {

                        final Row row = ar.result().iterator().next();

                        World world = new World(row.getInteger(0), row.getInteger(1));
                        sink.success(world);
                    }
                }));
    }

    private Mono<World> updateWorld(World world) {
        return Mono.create(sink -> {
            pgClients.getOne().preparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2", Tuple.of(world.randomnumber, world.id), ar -> {
                if (ar.failed()) {
                    sink.error(ar.cause());
                } else {
                    sink.success(world);
                }
            });
        });
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
        return Flux.create(sink ->
                pgClients.getOne().preparedQuery("SELECT * FROM fortune", ar -> {
                    if (ar.failed()) {
                        sink.error(ar.cause());
                        return;
                    }

                    PgIterator resultSet = ar.result().iterator();
                    while (resultSet.hasNext()) {
                        Tuple row = resultSet.next();
                        sink.next(new Fortune(row.getInteger(0), row.getString(1)));
                    }
                    sink.complete();
                }));
    }
}