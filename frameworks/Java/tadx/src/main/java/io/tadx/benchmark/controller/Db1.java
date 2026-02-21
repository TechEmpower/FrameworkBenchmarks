package io.tadx.benchmark.controller;

import io.tadx.benchmark.entity.World;
import io.tadx.core.data.DataMap;
import io.tadx.data.DbStorage;
import io.tadx.web.*;
import io.tadx.web.annotation.*;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.sqlclient.Tuple;

import java.util.SplittableRandom;

/**
 * EN: The entry point of the application.
 */

@RestController(mapping = "/db_rest_druid")
public class Db1 {


    private static final SplittableRandom RANDOM = new SplittableRandom();
    private final DbStorage dbStorage;

    public Db1(DbStorage dbStorage) {
        this.dbStorage = dbStorage;
    }

    //@RestFunction(method = HttpMethod.GET)
    public Future<World> execute() {
        Promise<World> promise = Promise.promise();
        World world = dbStorage.findEntity(World.class, Tuple.of(randomWorld()));
        promise.complete(world);
        return promise.future();
    }

    @RestFunction(method = HttpMethod.GET)
    public DataMap executeSQL() {
        return dbStorage.queryRow("SELECT id, randomnumber FROM world WHERE id = ?", Tuple.of(randomWorld()));
    }


    static int randomWorld() {
        return 1 + RANDOM.nextInt(10000);
    }

}
