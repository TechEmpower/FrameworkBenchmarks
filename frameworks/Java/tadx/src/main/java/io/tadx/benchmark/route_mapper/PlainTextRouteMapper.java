package io.tadx.benchmark.route_mapper;

import io.tadx.core.TadxApplication;
import io.tadx.web.TadxWebApplication;
import io.tadx.web.WebContext;
import io.tadx.web.annotation.RouteMapping;
import io.tadx.web.route_mapper.RouteMapper;
import io.vertx.core.MultiMap;
import io.vertx.core.http.HttpHeaders;


/**
 * CN: 请求映射结果的接口
 */
@RouteMapping(path = "/plaintext")
public class PlainTextRouteMapper implements RouteMapper {

    private MultiMap textHeaders;

    public PlainTextRouteMapper() {
        textHeaders();
        TadxApplication.vertx().setPeriodic(1000, id -> {
            textHeaders();
        });
    }

    @Override
    public void run(WebContext webContext) {
        webContext.routingContext().response().headers().addAll(textHeaders);
        webContext.routingContext().response().end("Hello, world!");
    }

    private void textHeaders() {
        textHeaders = HttpHeaders
                .headers()
                .add("Content-Type", "text/plain;charset=UTF-8")
                .add("Server", "Tad.x")
                .add("Date", TadxWebApplication.currentDateString)
                .copy(false);
    }
}
