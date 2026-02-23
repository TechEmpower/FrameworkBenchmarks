package io.tadx.benchmark.controller;

import io.tadx.benchmark.entity.World;
import io.tadx.data.DbStorage;
import io.tadx.web.HttpMethod;
import io.tadx.web.annotation.*;
import io.vertx.sqlclient.Tuple;

import java.util.SplittableRandom;
import java.util.concurrent.ConcurrentHashMap;

/**
 * EN: The entry point of the application.
 */

@RestController
public class CachedQueries {


    private static final SplittableRandom RANDOM = new SplittableRandom();

    private final static ConcurrentHashMap<Integer, World> cache = new ConcurrentHashMap<>();

    private final DbStorage dbStorage;

    public CachedQueries(DbStorage dbStorage) {
        this.dbStorage = dbStorage;
    }

    @RestFunction(mapping = "/cached_queries_rest", method = HttpMethod.GET)
    public World[] execute(int count) {
        if (count < 1) {
            count = 1;
        } else if (count > 500) {
            count = 500;
        }
        World[] worlds = new World[count];
        for (int i = 0; i < count; i++) {
            int id = randomWorld();
            worlds[i] = cache.computeIfAbsent(id, k -> dbStorage.findEntity(World.class, Tuple.of(id)));
        }
        return worlds;
    }

    static int randomWorld() {
        return 1 + RANDOM.nextInt(10000);
    }

}
