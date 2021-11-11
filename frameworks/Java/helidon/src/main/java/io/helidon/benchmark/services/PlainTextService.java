package io.helidon.benchmark.services;

import io.helidon.webserver.Routing;
import io.helidon.webserver.Service;

public class PlainTextService implements Service {

    @Override
    public void update(Routing.Rules rules) {
        rules.get("/plaintext",
                (req, res) -> res.send("Hello, World!"));
    }
}
