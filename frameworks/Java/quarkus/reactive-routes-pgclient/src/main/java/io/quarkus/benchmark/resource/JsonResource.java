package io.quarkus.benchmark.resource;

import javax.enterprise.context.ApplicationScoped;

import io.quarkus.vertx.web.Route;
import io.vertx.ext.web.RoutingContext;

@ApplicationScoped
public class JsonResource extends BaseResource {

    private static final String HELLO = "Hello, World!";

    @Route(path = "json")
    public void json(RoutingContext rc) {
        sendJson(rc, new Message(HELLO));
    }
}

