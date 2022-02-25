package io.helidon.benchmark.services;

import java.nio.charset.StandardCharsets;

import io.helidon.common.http.Http;
import io.helidon.common.http.MediaType;
import io.helidon.webserver.Handler;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;

public class PlainTextService implements Service, Handler {

    private static final byte[] MESSAGE = "Hello, World!".getBytes(StandardCharsets.UTF_8);
    private static final String MEDIA_TYPE = MediaType.TEXT_PLAIN.toString();

    @Override
    public void update(Routing.Rules rules) {
        rules.get("/plaintext", this);
    }

    @Override
    public void accept(ServerRequest req, ServerResponse res) {
        res.addHeader(Http.Header.CONTENT_TYPE, MEDIA_TYPE).send(MESSAGE);
    }
}