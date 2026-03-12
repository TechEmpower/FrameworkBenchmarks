package io.tadx.benchmark.route_mapper;

import io.tadx.core.TadxApplication;
import io.tadx.core.data.Json;
import io.tadx.web.TadxWebApplication;
import io.tadx.web.*;
import io.tadx.web.annotation.RouteMapping;
import io.tadx.web.route_mapper.RouteMapper;
import io.vertx.core.MultiMap;
import io.vertx.core.http.HttpHeaders;


/**
 * CN: 请求映射结果的接口
 */
@RouteMapping(path = "/json")
public class JsonRouteMapper implements RouteMapper {

    public static MultiMap jsonHeaders = jsonHeaders();

    public JsonRouteMapper() {
        TadxApplication.vertx().setPeriodic(1000, id -> {
            jsonHeaders = jsonHeaders();
        });
    }

    @Override
    public void run(WebContext webContext) {
        webContext.routingContext().response().headers().addAll(jsonHeaders);
        webContext.routingContext().response().end(Json.createNew().put("message", "Hello, World!").stringify());
    }

    private static MultiMap jsonHeaders() {
        return HttpHeaders
                .headers()
                .add("Content-Type", "application/json;charset=UTF-8")
                .add("Server", "Tad.x")
                .add("Date", TadxWebApplication.currentDateString)
                .copy(false);
    }
}
