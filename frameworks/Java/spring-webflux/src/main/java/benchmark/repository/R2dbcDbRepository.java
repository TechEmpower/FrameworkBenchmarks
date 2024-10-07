package benchmark.repository;

import org.springframework.context.annotation.Profile;
import org.springframework.r2dbc.core.DatabaseClient;
import org.springframework.stereotype.Component;

import benchmark.model.Fortune;
import benchmark.model.World;
import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactory;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Component
@Profile("r2dbc")
public class R2dbcDbRepository implements DbRepository {

    private final DatabaseClient databaseClient;
    private final ConnectionFactory connectionFactory;
    private final ThreadLocal<Mono<? extends Connection>> conn = new ThreadLocal<>();

    public R2dbcDbRepository(DatabaseClient databaseClient) {
        this.databaseClient = databaseClient;
        this.connectionFactory = databaseClient.getConnectionFactory();
    }

    @Override
    public Mono<World> getWorld(int id) {
        return databaseClient
                .sql("SELECT id, randomnumber FROM world WHERE id = $1")
                .bind("$1", id)
                .mapProperties(World.class)
                .first();
    }

    private Mono<World> updateWorld(World world) {
        return databaseClient
                .sql("UPDATE world SET randomnumber=$2 WHERE id = $1")
                .bind("$1", world.id)
                .bind("$2", world.randomnumber)
                .fetch()
                .rowsUpdated()
                .map(count -> world);
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
        return getConnection()
                .flatMapMany(conn -> conn.createStatement("SELECT id, message FROM " + "fortune").execute())
                .flatMap(result -> result.map(r -> new Fortune(r.get(0, Integer.class), r.get(1, String.class))));
    }

    private Mono<? extends Connection> getConnection() {
        if (this.conn.get() == null) {
            this.conn.set(Mono.from(connectionFactory.create()).cache());
        }
        return this.conn.get();
    }

}