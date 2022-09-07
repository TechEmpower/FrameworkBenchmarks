package io.helidon.benchmark.reactive.services;

import java.util.Collections;
import java.util.Map;

import jakarta.json.Json;
import jakarta.json.JsonBuilderFactory;

import io.helidon.reactive.webserver.Handler;
import io.helidon.reactive.webserver.Routing;
import io.helidon.reactive.webserver.ServerRequest;
import io.helidon.reactive.webserver.ServerResponse;
import io.helidon.reactive.webserver.Service;

public class JsonService implements Service, Handler {

    private static final String ATTR_NAME = "message";
    private static final String ATTR_VALUE = "Hello, World!";
    private static final Map<String, Object> ATTR_MAP = Map.of(ATTR_NAME, ATTR_VALUE);

    private final JsonBuilderFactory jsonBuilderFactory;

    public JsonService() {
        this.jsonBuilderFactory = Json.createBuilderFactory(Collections.emptyMap());
    }

    @Override
    public void update(Routing.Rules rules) {
        rules.get("/json", this);
    }

    @Override
    public void accept(ServerRequest req, ServerResponse res) {
        res.send(jsonBuilderFactory.createObjectBuilder(ATTR_MAP).build());
    }
}