package io.tadx.benchmark.controller;

import io.tadx.benchmark.db.PgConnPool;
import io.tadx.benchmark.entity.World;
import io.tadx.core.utils.AsyncUtils;
import io.tadx.web.HttpMethod;
import io.tadx.web.annotation.RestController;
import io.tadx.web.annotation.RestFunction;
import io.vertx.sqlclient.*;

import java.util.SplittableRandom;

/**
 * EN: The entry point of the application.
 */

@RestController(mapping = "/db3")
public class Db3 {
    private static final SplittableRandom RANDOM = new SplittableRandom();
    private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    private final PreparedQuery<RowSet<Row>> SELECT_WORLD_QUERY;

    public Db3() {

        SqlClient client = PgConnPool.client();
        SELECT_WORLD_QUERY = client.preparedQuery(SELECT_WORLD);
    }

    @RestFunction(method = HttpMethod.GET)
    public World execute() {
        RowSet<Row> rowSet = AsyncUtils.await(SELECT_WORLD_QUERY.execute(Tuple.of(randomWorld())));
        Row row = rowSet.iterator().next();
        World world = new World();
        world.id = row.getInteger("id");
        world.randomnumber = row.getInteger("randomnumber");
        return world;
    }


    static int randomWorld() {
        return 1 + RANDOM.nextInt(10000);
    }

}
