package io.helidon.benchmark.services;

import java.nio.charset.StandardCharsets;

import io.helidon.common.http.Http;
import io.helidon.common.media.type.MediaTypes;
import io.helidon.nima.webserver.http.HttpRules;
import io.helidon.nima.webserver.http.HttpService;
import io.helidon.nima.webserver.http.ServerRequest;
import io.helidon.nima.webserver.http.ServerResponse;

public class PlainTextService implements HttpService {

    private static final byte[] MESSAGE = "Hello, World!".getBytes(StandardCharsets.UTF_8);
    private static final String MEDIA_TYPE = MediaTypes.TEXT_PLAIN.text();

    public void accept(ServerRequest req, ServerResponse res) {
        res.header(Http.Header.CONTENT_TYPE, MEDIA_TYPE).send(MESSAGE);
    }

    @Override
    public void routing(HttpRules httpRules) {
        httpRules.get("/", this::accept);
    }
}