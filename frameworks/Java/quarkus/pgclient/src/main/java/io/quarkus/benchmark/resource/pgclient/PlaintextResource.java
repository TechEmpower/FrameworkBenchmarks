package io.quarkus.benchmark.resource.pgclient;

import javax.enterprise.context.ApplicationScoped;

import io.quarkus.vertx.web.Route;
import io.vertx.core.buffer.Buffer;
import io.vertx.ext.web.RoutingContext;

@ApplicationScoped
public class PlaintextResource {
    private static final Buffer HELLO = Buffer.buffer("Hello, World!");

    @Route(path = "plaintext")
    public void plaintext(RoutingContext rc) {
        rc.response().putHeader("Content-Type", "text/plain");
        rc.response().end(HELLO);
    }
}
