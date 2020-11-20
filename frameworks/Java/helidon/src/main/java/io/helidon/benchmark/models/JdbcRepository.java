package io.helidon.benchmark.models;

import io.helidon.common.reactive.Multi;
import io.helidon.common.reactive.Single;
import io.helidon.dbclient.DbClient;

public class JdbcRepository implements DbRepository {
    private final DbClient client;

    public JdbcRepository(DbClient client) {
        this.client = client;
    }

    @Override
    public Single<World> getWorld(int id) {
        return this.client.execute(exec -> exec
            .createNamedGet("select-world-by-id")
            .params(id)
            .execute()
        ).map(o -> o.map(r -> new World(r.column(1).as(Integer.class), r.column(2).as(Integer.class))).get());
    }

    @Override
    public Single<World> updateWorld(World world) {
        return this.client.execute(exec -> exec
            .createNamedUpdate("update-world-by-id")
            .params(world.randomNumber, world.id)
            .execute()
        ).map(r -> world);
    }

    @Override
    public Multi<Fortune> getFortunes() {
        return this.client.execute(exec -> exec
            .createNamedQuery("select-all-fortune")
            .execute()
        ).map(r -> new Fortune(r.column(1).as(Integer.class), r.column(2).as(String.class)));
    }
}
