package com.kartashov.fb.tuprolog;

import io.vertx.core.Vertx;

public class Runner {
    public static void main(String... args) {
        var engine = new Engine(args);
        var server = Vertx.vertx().createHttpServer();
        server.requestHandler(engine::resolve);
        server.listen(8080);
    }
}
