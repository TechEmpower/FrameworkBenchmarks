package io.tadx.benchmark.route_mapper;

import io.tadx.core.data.Json;
import io.tadx.web.TadxWebApplication;
import io.tadx.web.*;
import io.tadx.web.annotation.RouteMapping;
import io.tadx.web.route.RouteMapper;


/**
 * CN: 请求映射结果的接口
 */
@RouteMapping(path = "/json")
public class JsonRouteMapper implements RouteMapper {

    @Override
    public void run(WebContext webContext) {
        webContext.routingContext().response()
                .putHeader("Content-Type", "application/json;charset=UTF-8")
                .putHeader("Server", "Tad.x")
                .putHeader("Date", TadxWebApplication.currentDateString)
                .end(Json.createNew().put("message", "Hello, World!").stringify());
    }
}
