package io.tadx.benchmark.route_mapper;

import io.tadx.benchmark.entity.Fortune;
import io.tadx.core.TadxApplication;
import io.tadx.core.utils.Utils;
import io.tadx.web.TadxWebApplication;
import io.tadx.web.WebContext;
import io.tadx.web.annotation.RouteMapping;
import io.tadx.web.route.RouteMapper;
import io.vertx.pgclient.PgBuilder;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.sqlclient.PoolOptions;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.Tuple;
import io.vertx.sqlclient.impl.SqlClientInternal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


/**
 * RouteMapper+Postgresql Client 模式的/queries测试（多查询时性能低于批量执行）
 */
@RouteMapping(path = "/fortunes2")
public class FortunesRouteMapper2 implements RouteMapper {

    PgConnectOptions connectOptions;
    PoolOptions poolOptions;
    SqlClientInternal client;
    private static final String SELECT_FORTUNES = "SELECT id, message FROM fortune";

    public FortunesRouteMapper2() {
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
    }

    @Override
    public void run(WebContext webContext) {
        client.preparedQuery(SELECT_FORTUNES).execute(Tuple.tuple()).onComplete(ar -> {
            if (ar.succeeded()) {
                List<Fortune> fortunes = new ArrayList<>();

                for (Row row : ar.result().value()) {
                    Fortune fortune = new Fortune();
                    fortune.id = row.getInteger("id");
                    fortune.message = row.getString("message");
                    fortunes.add(fortune);
                }

                fortunes.addFirst(new Fortune(0, "Additional fortune added at request time."));
                Collections.sort(fortunes);

                StringBuilder sb = new StringBuilder("<!DOCTYPE html><html lang=\"en-US\"><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>");
                for (Fortune fortune : fortunes) {
                    sb.append("<tr><td>").append(fortune.id).append("</td><td>").append(Utils.htmlEncode(fortune.message)).append("</td></tr>");
                }
                sb.append("</table></body></html>");

                webContext.routingContext().response()
                        .putHeader("Content-Type", "text/html;charset=UTF-8")
                        .putHeader("Server", "Tad.x")
                        .putHeader("Date", TadxWebApplication.currentDateString).end(sb.toString());
            }
        });
    }
}
