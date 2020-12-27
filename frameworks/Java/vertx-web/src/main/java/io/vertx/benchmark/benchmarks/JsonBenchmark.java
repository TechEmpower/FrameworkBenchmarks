package io.vertx.benchmark.benchmarks;

import io.vertx.benchmark.model.Message;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.Json;
import io.vertx.ext.web.RoutingContext;

public class JsonBenchmark extends BaseBenchmark{

    public JsonBenchmark(Vertx vertx) {
        super(vertx);
    }

    public void json(RoutingContext ctx){
        ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                .end(Json.encodeToBuffer(new Message("Hello, World!")));
    }

}
