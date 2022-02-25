package io.quarkus.benchmark.repository;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;

import io.quarkus.benchmark.model.World;
import io.smallrye.mutiny.Uni;
import io.vertx.mutiny.sqlclient.Row;
import io.vertx.mutiny.sqlclient.Tuple;

@ApplicationScoped
public class WorldRepository {

    @Inject
    PgClients clients;

    public Uni<World> find(int id) {
        return clients.getClient().preparedQuery("SELECT id, randomNumber FROM World WHERE id = $1")
                .execute(Tuple.of(id))
                .map(rowset -> {
                    Row row = rowset.iterator().next();
                    return new World(row.getInteger(0), row.getInteger(1));
                });
    }

    public Uni<Void> update(World[] worlds) {
        Arrays.sort(worlds);
        List<Tuple> args = new ArrayList<>(worlds.length);
        for (World world : worlds) {
            args.add(Tuple.of(world.getId(), world.getRandomNumber()));
        }
        return clients.getPool().preparedQuery("UPDATE World SET randomNumber = $2 WHERE id = $1")
                .executeBatch(args)
                .map(v -> null);
    }
}
