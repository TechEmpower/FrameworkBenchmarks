package io.tadx.benchmark.route_mapper;

import io.tadx.benchmark.db.PgConnPool;
import io.tadx.benchmark.entity.World;
import io.tadx.core.data.Json;
import io.tadx.web.TadxWebApplication;
import io.tadx.web.WebContext;
import io.tadx.web.annotation.RouteMapping;
import io.tadx.web.route.RouteMapper;
import io.vertx.sqlclient.*;
import io.vertx.sqlclient.impl.SqlClientInternal;

import java.util.ArrayList;
import java.util.List;
import java.util.SplittableRandom;
import java.util.concurrent.atomic.AtomicInteger;


/**
 * RouteMapper+Postgresql Client 模式的/queries测试（多查询时性能低于批量执行）
 */
@RouteMapping(path = "/update")
public class UpdateMapper implements RouteMapper {

    private static final SplittableRandom RANDOM = new SplittableRandom();
    SqlClientInternal client;
    private static final String SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1";
    //private final PreparedQuery<RowSet<Row>> SELECT_WORLD_QUERY;

    public UpdateMapper() {
        client = PgConnPool.client();
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
                    List<Integer> params = new ArrayList<>();
                    client.preparedQuery(buildUpdateQuery(worlds, params)).execute(Tuple.wrap(params)).onComplete(ar2 -> {
                        if (ar2.succeeded()) {
                            webContext.routingContext().response()
                                    .putHeader("Content-Type", "application/json;charset=UTF-8")
                                    .putHeader("Server", "Tad.x")
                                    .putHeader("Date", TadxWebApplication.currentDateString)
                                    .end(Json.stringify(worlds));
                        } else {
                            webContext.response().end(ar2.cause().getMessage());
                        }
                    });
                }
            });
        }
    }


    private static String buildUpdateQuery(List<World> worlds, List<Integer> params) {
        StringBuilder sql = new StringBuilder();
        sql.append("UPDATE world SET randomnumber = CASE ID");
        for (int i = 0; i < worlds.size(); i++) {
            int offset = (i * 2) + 1;
            sql.append(" WHEN $").append(offset).append(" THEN $").append(offset + 1);
            params.add(worlds.get(i).id);
            params.add(randomWorld());
        }
        sql.append(" ELSE randomnumber");
        sql.append(" END WHERE ID IN ($1");
        for (int i = 1; i < worlds.size(); i++) {
            int offset = (i * 2) + 1;
            sql.append(",$").append(offset);
        }
        sql.append(")");
        return sql.toString();
    }

    static int randomWorld() {
        return 1 + RANDOM.nextInt(10000);
    }
}
