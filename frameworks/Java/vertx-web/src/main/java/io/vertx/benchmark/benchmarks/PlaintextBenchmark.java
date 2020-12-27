package io.vertx.benchmark.benchmarks;

import io.vertx.core.Vertx;
import io.vertx.core.http.HttpHeaders;
import io.vertx.ext.web.RoutingContext;

public class PlaintextBenchmark extends BaseBenchmark {
    public PlaintextBenchmark(Vertx vertx) {
        super(vertx);
    }

    public void plaintext(RoutingContext ctx) {
        ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "text/plain")
                .end("Hello, World!");
    }
}
