package io.helidon.benchmark.services;

import java.util.Collections;

import javax.json.Json;
import javax.json.JsonBuilderFactory;

import io.helidon.webserver.Routing;
import io.helidon.webserver.Service;

public class JsonService implements Service {

    private JsonBuilderFactory jsonBuilderFactory;

     public JsonService() {
         this.jsonBuilderFactory = Json.createBuilderFactory(Collections.emptyMap());
     }

    @Override
    public void update(Routing.Rules rules) {
        rules.get("/json",
                (req, res) -> res.send(jsonBuilderFactory.createObjectBuilder(Collections.singletonMap("message", "Hello, World!")).build()));
    }
}
