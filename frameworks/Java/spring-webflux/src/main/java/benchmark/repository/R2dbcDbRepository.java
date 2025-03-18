package benchmark.repository;

import java.util.List;

import org.springframework.context.annotation.Profile;
import org.springframework.r2dbc.core.DatabaseClient;
import org.springframework.stereotype.Component;

import benchmark.model.Fortune;
import benchmark.model.World;
import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.Result;
import io.r2dbc.spi.Statement;
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
        return databaseClient
                .sql("SELECT id, randomnumber FROM world WHERE id = $1")
                .bind(0, id)
                .mapProperties(World.class)
                .first();
    }

    @Override
    public Mono<Void> updateWorlds(List<World> worlds) {
        return databaseClient.inConnectionMany(con -> {
            Statement statement = con.createStatement("UPDATE world SET randomnumber=$2 WHERE id = $1");
            for (int i = 0; i < worlds.size(); i++) {
                World world = worlds.get(i);
                statement.bind(0, world.randomnumber)
                        .bind(1, world.id);
                if (i < worlds.size() - 1) {
                    statement.add();
                }
            }
            return Flux.from(statement.execute());
        }).flatMap(Result::getRowsUpdated).then();
    }

    @Override
    public Flux<Fortune> fortunes() {
        return databaseClient
                .sql("SELECT id, message FROM fortune")
                .mapProperties(Fortune.class)
                .all();
    }

}