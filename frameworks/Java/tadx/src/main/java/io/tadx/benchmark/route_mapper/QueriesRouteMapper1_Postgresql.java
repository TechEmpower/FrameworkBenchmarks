package io.tadx.benchmark.route_mapper;

import io.tadx.benchmark.entity.World;
import io.tadx.core.TadxApplication;
import io.tadx.core.data.Json;
import io.tadx.web.TadxWebApplication;
import io.tadx.web.WebContext;
import io.tadx.web.annotation.RouteMapping;
import io.tadx.web.route.RouteMapper;
import io.vertx.pgclient.PgBuilder;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.sqlclient.*;
import io.vertx.sqlclient.impl.SqlClientInternal;

import java.util.ArrayList;
import java.util.List;
import java.util.SplittableRandom;
import java.util.concurrent.atomic.AtomicInteger;


/**
 * RouteMapper+Postgresql Client 模式的/queries测试（多查询时性能低于批量执行）
 */
@RouteMapping(path = "/queries")
public class QueriesRouteMapper1_Postgresql implements RouteMapper {

    private static final SplittableRandom RANDOM = new SplittableRandom();
    PgConnectOptions connectOptions;
    PoolOptions poolOptions;
    SqlClientInternal client;
    private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    //private final PreparedQuery<RowSet<Row>> SELECT_WORLD_QUERY;

    public QueriesRouteMapper1_Postgresql() {
        connectOptions = new PgConnectOptions().
                setPort(5432).setHost("tfb-database").
                setDatabase("hello_world").
                setUser("benchmarkdbuser").
                setPassword("benchmarkdbpass").
                setCachePreparedStatements(true).
                setPreparedStatementCacheMaxSize(1024).
                setPipeliningLimit(100000);
        // Pool options
        poolOptions = new PoolOptions().setMaxSize(2000);
        // Create the client pool
        client = (SqlClientInternal) PgBuilder.client().with(poolOptions).connectingTo(connectOptions).using(TadxApplication.vertx()).build();
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
        int count = queries;



        List<World> worlds = new ArrayList<>();
        for (AtomicInteger i = new AtomicInteger(); i.get() < queries; i.getAndIncrement()) {
            client.preparedQuery(SELECT_WORLD).execute(Tuple.of(randomWorld())).onComplete(ar -> {
                if (ar.succeeded()) {
                    Row row = ar.result().iterator().next();
                    World world = new World();
                    world.id = row.getInteger("id");
                    world.randomnumber = row.getInteger("randomnumber");
                    worlds.add(world);
                } else {
                    System.out.println("*******************: "+ar.cause());
                    i.getAndDecrement();
                }
                if (worlds.size() == count) {
                    webContext.routingContext().response()
                            .putHeader("Content-Type", "application/json;charset=UTF-8")
                            .putHeader("Server", "Tad.x")
                            .putHeader("Date", TadxWebApplication.currentDateString)
                            .end(Json.stringify(worlds));
                }
            });
        }
    }

    static int randomWorld() {
        return 1 + RANDOM.nextInt(10000);
    }
}
