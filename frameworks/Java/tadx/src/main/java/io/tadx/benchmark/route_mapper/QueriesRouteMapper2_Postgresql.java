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
import io.vertx.sqlclient.Tuple;
import io.vertx.sqlclient.impl.SqlClientInternal;

import java.util.ArrayList;
import java.util.List;
import java.util.SplittableRandom;


/**
 * RouteMapper+Postgresql Client 模式的/queries测试
 */
@RouteMapping(path = "/queries")
public class QueriesRouteMapper2_Postgresql implements RouteMapper {

    private static final SplittableRandom RANDOM = new SplittableRandom();
    SqlClientInternal client;
    private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    //private final PreparedQuery<RowSet<Row>> SELECT_WORLD_QUERY;

    public QueriesRouteMapper2_Postgresql() {

        // Create the client pool
        client = PgConnPool.client();;
        //SELECT_WORLD_QUERY = client.preparedQuery(SELECT_WORLD);
    }

    @Override
    public void run(WebContext webContext) {
        int queries = webContext.request().readInt("queries", 1);
        if (queries < 1) {
            queries = 1;
        } else if (queries > 500) {
            queries = 500;
        }

        List<Tuple> batch = new ArrayList<>();
        int count = queries;

        for (int i = 0; i < queries; i++) {
            batch.add(Tuple.of(randomWorld()));
        }
        client.preparedQuery(SELECT_WORLD)
                .executeBatch(batch)
                .onComplete(res -> {
                    if (res.succeeded()) {
                        // Process rows
                        RowSet<Row> rows = res.result();
                        List<World> worlds = new ArrayList<>();
                        while (rows !=null) {
                            Row row = rows.iterator().next();
                            World world = new World();
                            world.id = row.getInteger("id");
                            world.randomnumber = row.getInteger("randomnumber");
                            worlds.add(world);
                            rows = rows.next();
                        }

                        webContext.routingContext().response()
                                .putHeader("Content-Type", "application/json;charset=UTF-8")
                                .putHeader("Server", "Tad.x")
                                .putHeader("Date", TadxWebApplication.currentDateString)
                                .end(Json.stringify(worlds));
                    }
                });
    }

    static int randomWorld() {
        return 1 + RANDOM.nextInt(10000);
    }
}
