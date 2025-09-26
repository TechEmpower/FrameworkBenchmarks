package io.helidon.benchmark.services;

import java.util.Collections;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonBuilderFactory;

import io.helidon.webserver.Handler;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;

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