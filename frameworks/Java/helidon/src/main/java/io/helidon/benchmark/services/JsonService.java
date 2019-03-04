package io.helidon.benchmark.services;

import io.helidon.media.jsonp.server.JsonSupport;
import io.helidon.webserver.Routing;
import io.helidon.webserver.Service;

import javax.json.Json;
import java.util.Collections;

public class JsonService implements Service {

    @Override
    public void update(Routing.Rules rules) {
        rules.register("/json", JsonSupport.create());
        rules.get("/json",
                (req, res) -> res.send(Json.createObjectBuilder(Collections.singletonMap("message", "Hello, World!")).build()));
    }
}
