package io.tadx.benchmark.route_mapper;

import io.tadx.benchmark.db.PgConnPool;
import io.tadx.web.TadxWebApplication;
import io.tadx.web.WebContext;
import io.tadx.web.WebResult;
import io.tadx.web.annotation.RouteMapping;
import io.tadx.web.route.RouteMapper;
import io.vertx.sqlclient.*;

import java.util.SplittableRandom;


/**
 * RouteMapper+Postgresql Client 模式的/db测试
 */
@RouteMapping(path = "/db")
public class DbRouteMapper_Postgresql implements RouteMapper {

    private static final SplittableRandom RANDOM = new SplittableRandom();
    SqlClient client;
    private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    //private final PreparedQuery<RowSet<Row>> SELECT_WORLD_QUERY;

    public DbRouteMapper_Postgresql() {
        // Create the client pool
        client = PgConnPool.client();
        //SELECT_WORLD_QUERY = client.preparedQuery(SELECT_WORLD);
    }

    @Override
    public void run(WebContext webContext) {
        //client.preparedQuery(SELECT_WORLD)的性能高于SELECT_WORLD_QUERY.execute，但整体差异不大
        client.preparedQuery(SELECT_WORLD).execute(Tuple.of(randomWorld())).onComplete(ar -> {
            if (ar.succeeded()) {
                webContext.routingContext().response()
                        .putHeader("Content-Type", "application/json;charset=UTF-8")
                        .putHeader("Server", "Tad.x")
                        .putHeader("Date", TadxWebApplication.currentDateString)
                        .end(ar.result().iterator().next().toJson().toString());
            } else {
                webContext.exception(WebResult.errorResult(ar.cause().getMessage()));
            }
            //client.close();
        });
    }

    static int randomWorld() {
        return 1 + RANDOM.nextInt(10000);
    }
}
