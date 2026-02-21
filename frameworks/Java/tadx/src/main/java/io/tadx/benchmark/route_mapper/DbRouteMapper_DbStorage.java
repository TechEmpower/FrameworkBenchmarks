package io.tadx.benchmark.route_mapper;

import io.tadx.core.data.DataMap;
import io.tadx.data.DbStorage;
import io.tadx.web.TadxWebApplication;
import io.tadx.web.WebContext;
import io.tadx.web.annotation.RouteMapping;
import io.tadx.web.route.RouteMapper;
import io.vertx.sqlclient.Tuple;

import java.util.SplittableRandom;


/**
 * RouteMapper+DbStorage模式的/db测试
 * 备忘：实际性能与RestController不相上下（RouteMapper的）
 */
@RouteMapping(path = "/db_storage")
public class DbRouteMapper_DbStorage implements RouteMapper {


    private static final SplittableRandom RANDOM = new SplittableRandom();
    private final DbStorage dbStorage;

    public DbRouteMapper_DbStorage(DbStorage dbStorage) {
        this.dbStorage = dbStorage;
    }

    @Override
    public void run(WebContext webContext) {
        //World world = dbStorage.findEntity(World.class, randomWorld());
        DataMap row = dbStorage.queryRow("SELECT id, randomnumber FROM world WHERE id = ?", Tuple.of(randomWorld()));

        webContext.routingContext().response()
                .putHeader("Content-Type", "application/json;charset=UTF-8")
                .putHeader("Server", "Tad.x")
                .putHeader("Date", TadxWebApplication.currentDateString)
                .end(row.toString());
                //.end(world.toJsonString());
    }

    static int randomWorld() {
        return 1 + RANDOM.nextInt(10000);
    }
}
