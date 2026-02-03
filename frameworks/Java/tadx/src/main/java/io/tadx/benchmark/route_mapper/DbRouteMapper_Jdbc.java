package io.tadx.benchmark.route_mapper;

import io.tadx.core.TadxApplication;
import io.tadx.web.TadxWebApplication;
import io.tadx.web.WebContext;
import io.tadx.web.WebResult;
import io.tadx.web.annotation.RouteMapping;
import io.tadx.web.route.RouteMapper;
import io.vertx.jdbcclient.JDBCConnectOptions;
import io.vertx.jdbcclient.JDBCPool;
import io.vertx.sqlclient.*;

import java.util.SplittableRandom;


/**
 * RouteMapper+Jdbc Client 模式的/db测试
 */
@RouteMapping(path = "/db_jdbc")
public class DbRouteMapper_Jdbc implements RouteMapper {

    private static final SplittableRandom RANDOM = new SplittableRandom();
    JDBCConnectOptions connectOptions;
    PoolOptions poolOptions;
    private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=?";
    private final PreparedQuery<RowSet<Row>> SELECT_WORLD_QUERY;
    Pool pool;

    public DbRouteMapper_Jdbc() {
        connectOptions = new JDBCConnectOptions()
                .setJdbcUrl("jdbc:postgresql://tfb-database:5432/hello_world")
                .setUser("benchmarkdbuser")
                .setPassword("benchmarkdbpass");
        // Pool options
        poolOptions = new PoolOptions().setMaxSize(200);
        // Create the client pool
        pool = JDBCPool.pool(TadxApplication.vertx(), connectOptions, poolOptions);
        SELECT_WORLD_QUERY = pool.preparedQuery(SELECT_WORLD);
    }

    @Override
    public void run(WebContext webContext) {
        pool.preparedQuery(SELECT_WORLD).execute(Tuple.of(randomWorld())).onComplete(ar -> {
            if (ar.succeeded()) {
                RowSet<Row> rows = ar.result();
                webContext.routingContext().response()
                        .putHeader("Content-Type", "application/json;charset=UTF-8")
                        .putHeader("Server", "Tad.x")
                        .putHeader("Date", TadxWebApplication.currentDateString)
                        .end(rows.iterator().next().toJson().toString());
            } else {
                webContext.exception(WebResult.errorResult(ar.cause().getMessage()));
            }
        });
    }

    static int randomWorld() {
        return 1 + RANDOM.nextInt(10000);
    }
}
