package io.helidon.benchmark.reactive.services;

import java.nio.charset.StandardCharsets;

import io.helidon.common.http.Http;
import io.helidon.common.media.type.MediaTypes;
import io.helidon.reactive.webserver.Handler;
import io.helidon.reactive.webserver.Routing;
import io.helidon.reactive.webserver.ServerRequest;
import io.helidon.reactive.webserver.ServerResponse;
import io.helidon.reactive.webserver.Service;

public class PlainTextService implements Service, Handler {

    private static final byte[] MESSAGE = "Hello, World!".getBytes(StandardCharsets.UTF_8);
    private static final String MEDIA_TYPE = MediaTypes.TEXT_PLAIN.text();

    @Override
    public void update(Routing.Rules rules) {
        rules.get("/plaintext", this);
    }

    @Override
    public void accept(ServerRequest req, ServerResponse res) {
        res.addHeader(Http.Header.CONTENT_TYPE, MEDIA_TYPE).send(MESSAGE);
    }
}