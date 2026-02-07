package io.tadx.benchmark.controller;

import io.tadx.benchmark.entity.World;
import io.tadx.data.DbStorage;
import io.tadx.web.HttpMethod;
import io.tadx.web.annotation.*;

import java.util.SplittableRandom;

/**
 * EN: The entry point of the application.
 */

@RestController(mapping = "/queries_rest")
public class Queries {


    private static final SplittableRandom RANDOM = new SplittableRandom();
    private final DbStorage dbStorage;

    public Queries(DbStorage dbStorage) {
        this.dbStorage = dbStorage;
    }

    @RestFunction(method = HttpMethod.GET)
    public World[] execute(int queries) {
        if (queries < 1) {
            queries = 1;
        } else if (queries > 500) {
            queries = 500;
        }
        World[] worlds = new World[queries];
        for (int i = 0; i < queries; i++) {
            worlds[i] = dbStorage.findEntity(World.class, randomWorld());
        }
        return worlds;
    }

    static int randomWorld() {
        return 1 + RANDOM.nextInt(10000);
    }

}
