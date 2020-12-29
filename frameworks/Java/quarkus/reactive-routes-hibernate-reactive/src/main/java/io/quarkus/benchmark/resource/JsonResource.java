package io.quarkus.benchmark.resource;

import java.util.Collections;

import javax.enterprise.context.ApplicationScoped;

import io.quarkus.vertx.web.Route;
import io.vertx.ext.web.RoutingContext;

@ApplicationScoped
public class JsonResource extends BaseResource {

    private static final String MESSAGE = "message";
    private static final String HELLO = "Hello, World!";

    @Route(path = "json")
    public void json(RoutingContext rc) {
        sendJson(rc, Collections.singletonMap( MESSAGE, HELLO ));
    }
}

