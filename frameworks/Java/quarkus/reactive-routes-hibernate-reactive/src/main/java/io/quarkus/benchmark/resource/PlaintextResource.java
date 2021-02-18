package io.quarkus.benchmark.resource;

import javax.enterprise.context.ApplicationScoped;

import io.quarkus.vertx.web.Route;
import io.vertx.ext.web.RoutingContext;

@ApplicationScoped
public class PlaintextResource {
    private static final String HELLO = "Hello, World!";

    @Route(path = "plaintext")
    public void plaintext(RoutingContext rc) {
        rc.response().putHeader("Content-Type", "text/plain");
        rc.response().end(HELLO);
    }
}
