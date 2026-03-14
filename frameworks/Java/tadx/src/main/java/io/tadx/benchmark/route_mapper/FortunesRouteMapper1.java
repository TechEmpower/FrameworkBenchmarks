package io.tadx.benchmark.route_mapper;

import io.tadx.benchmark.db.PgConnPool;
import io.tadx.benchmark.entity.Fortune;
import io.tadx.web.TadxWebApplication;
import io.tadx.web.WebContext;
import io.tadx.web.WebResult;
import io.tadx.web.annotation.RouteMapping;
import io.tadx.web.template.FreemarkerEngine;
import io.tadx.web.route.RouteMapper;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.Tuple;
import io.vertx.sqlclient.impl.SqlClientInternal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


/**
 * RouteMapper+Postgresql Client 模式的/queries测试（多查询时性能低于批量执行）
 */
@RouteMapping(path = "/fortunes")
public class FortunesRouteMapper1 implements RouteMapper {

    SqlClientInternal client;
    private static final String SELECT_FORTUNES = "SELECT id, message FROM fortune";

    public FortunesRouteMapper1() {
        // Create the client pool
        client = PgConnPool.client();
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

                webContext.routingContext().response()
                        .putHeader("Content-Type", "application/json;charset=UTF-8")
                        .putHeader("Server", "Tad.x")
                        .putHeader("Date", TadxWebApplication.currentDateString);
                webContext.complete(WebResult.pageResult(FreemarkerEngine.class).put("fortunes", fortunes).templateContent("""
                        <!DOCTYPE html>
                               <html lang="en-US">
                               <head>
                                   <title>Fortunes</title>
                               </head>
                               <body>
                               <table>
                                   <tr>
                                       <th>id</th>
                                       <th>message</th>
                                   </tr>
                                   <#list fortunes as fortune>
                                   <tr>
                                       <td>${fortune.id}</td>
                                       <td>${fortune.message?html}</td>
                                   </tr>
                                   </#list>
                               </table>
                               </body>
                               </html>"""));
            }
        });
    }
}
