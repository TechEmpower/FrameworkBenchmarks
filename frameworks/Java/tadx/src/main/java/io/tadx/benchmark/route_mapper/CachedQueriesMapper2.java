package io.tadx.benchmark.route_mapper;

import io.tadx.benchmark.db.PgConnPool;
import io.tadx.benchmark.entity.World;
import io.tadx.core.data.Json;
import io.tadx.web.TadxWebApplication;
import io.tadx.web.WebContext;
import io.tadx.web.annotation.RouteMapping;
import io.tadx.web.route.RouteMapper;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.impl.SqlClientInternal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.SplittableRandom;
import java.util.concurrent.atomic.AtomicInteger;


/**
 * EN: Use HashMap to cache <br/>
 * CN: 使用HashMap缓存
 */
@RouteMapping(path = "/cached_queries2")
public class CachedQueriesMapper2 implements RouteMapper {

    private static final SplittableRandom RANDOM = new SplittableRandom();
    SqlClientInternal client;
    private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD";

    private final static HashMap<Integer, World> cache = new HashMap<>();

    public CachedQueriesMapper2() {

        // Create the client pool
        client = PgConnPool.client();
        //SELECT_WORLD_QUERY = client.preparedQuery(SELECT_WORLD);

        AtomicInteger key = new AtomicInteger();

        client.preparedQuery(SELECT_WORLD).execute().onComplete(ar -> {
            if (ar.succeeded()) {
                RowSet<Row> rows = ar.result().value();
                for (Row row : rows) {
                    World world = new World();
                    world.id = row.getInteger("id");
                    world.randomnumber = row.getInteger("randomnumber");
                    cache.put(key.getAndIncrement(), world);
                }
            }
        });
    }

    @Override
    public void run(WebContext webContext) {
        int count = webContext.request().readInt("count", 1);
        if (count < 1) {
            count = 1;
        } else if (count > 500) {
            count = 500;
        }

        List<World> worlds = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            int id = randomWorld();
            worlds.add(cache.get(id));
        }
        webContext.routingContext().response()
                .putHeader("Content-Type", "application/json;charset=UTF-8")
                .putHeader("Server", "Tad.x")
                .putHeader("Date", TadxWebApplication.currentDateString)
                .end(Json.stringify(worlds));
    }

    static int randomWorld() {
        return 1 + RANDOM.nextInt(1000);
    }
}
