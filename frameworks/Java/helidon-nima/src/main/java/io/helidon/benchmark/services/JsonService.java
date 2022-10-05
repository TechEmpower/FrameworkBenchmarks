package io.helidon.benchmark.services;

import io.helidon.common.media.type.MediaTypes;
import io.helidon.nima.webserver.http.HttpRules;
import io.helidon.nima.webserver.http.HttpService;
import io.helidon.nima.webserver.http.ServerRequest;
import io.helidon.nima.webserver.http.ServerResponse;

import javax.json.Json;

public class JsonService implements HttpService {

    private static final String ATTR_NAME = "message";
    private static final String ATTR_VALUE = "Hello, World!";

    public void accept(ServerRequest req, ServerResponse res) {
        res.headers().contentType(MediaTypes.APPLICATION_JSON);
        res.send(Json.createObjectBuilder().add(ATTR_NAME, ATTR_VALUE).build().toString());
    }

    @Override
    public void routing(HttpRules httpRules) {
        httpRules.get("/", this::accept);
    }
}