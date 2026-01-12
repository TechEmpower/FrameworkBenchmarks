package io.tadx.benchmark.route_mapper;

import io.tadx.web.TadxWebApplication;
import io.tadx.web.WebContext;
import io.tadx.web.*;
import io.tadx.web.annotation.RouteMapping;
import io.tadx.web.route.RouteMapper;
import io.vertx.core.Future;
import io.vertx.ext.web.RoutingContext;

import java.util.List;


/**
 * CN: 请求映射结果的接口
 */
@RouteMapping(path = "/plaintext")
public class PlainTextRouteMapper implements RouteMapper {

    @Override
    public void run(WebContext webContext) {
        webContext.routingContext().response()
                .putHeader("Content-Type", "text/plain;charset=UTF-8")
                .putHeader("Server", "Tad.x")
                .putHeader("Date", TadxWebApplication.currentDateString)
                .end("Hello, world!");
    }
}
