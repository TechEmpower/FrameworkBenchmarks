package io.quarkus.benchmark.repository;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import io.quarkus.benchmark.model.World;
import io.smallrye.mutiny.Uni;
import io.vertx.core.json.JsonObject;
import io.vertx.mutiny.sqlclient.Row;
import io.vertx.mutiny.sqlclient.Tuple;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

@Singleton
public class WorldRepository {

    //like muggle's JsonWorld
    public static class JsonWorld extends JsonObject {
        public JsonWorld(final Integer id, final Integer randomNumber) {
            super(Map.of("id", id, "randomNumber", randomNumber));
        }
    }


    @Inject
    PgClients clients;


    public Uni<JsonWorld> findAsJsonWorld(final Integer id) {
        return clients.getClient().preparedQuery("SELECT id, randomNumber FROM World WHERE id = $1")
                .execute(Tuple.of(id))
                .map(rowset -> {
                    final Row row = rowset.iterator().next();
                    return new JsonWorld(row.getInteger(0), row.getInteger(1));
                });
    }

    public Uni<World> find(final Integer id) {
        return clients.getClient().preparedQuery("SELECT id, randomNumber FROM World WHERE id = $1")
                .execute(Tuple.of(id))
                .map(rowset -> {
                    final Row row = rowset.iterator().next();
                    return new World(row.getInteger(0), row.getInteger(1));
                });
    }

    public Uni<Void> update(final World[] worlds) {
        Arrays.sort(worlds);
        final List<Tuple> args = new ArrayList<>(worlds.length);
        for (final World world : worlds) {
            args.add(Tuple.of(world.getId(), world.getRandomNumber()));
        }
        return clients.getClient().preparedQuery("UPDATE World SET randomNumber = $2 WHERE id = $1")
                .executeBatch(args)
                .map(v -> null);
    }
}
