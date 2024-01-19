package io.quarkus.benchmark.resource;


import io.quarkus.vertx.web.Route;
import io.vertx.ext.web.RoutingContext;
import jakarta.inject.Singleton;

@Singleton
public class JsonResource extends BaseResource {

    private static final String HELLO = "Hello, World!";

    @Route(path = "json")
    public void json(final RoutingContext rc) {
        sendJson(rc, new JsonMessage(HELLO));
    }
}
