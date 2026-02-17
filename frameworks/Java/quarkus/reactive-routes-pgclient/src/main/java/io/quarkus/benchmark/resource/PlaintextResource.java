package io.quarkus.benchmark.resource;

import io.netty.handler.codec.http.HttpHeaderValues;
import io.quarkus.vertx.web.Route;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpHeaders;
import io.vertx.ext.web.RoutingContext;
import jakarta.inject.Singleton;

@Singleton
public class PlaintextResource {
    private static final String HELLO_WORLD = "Hello, world!";
    private static final Buffer HELLO_WORLD_BUFFER = Buffer.buffer(HELLO_WORLD, "UTF-8");

    @Route(path = "plaintext")
    public void plaintext(final RoutingContext rc) {
        final var response = rc.response();
        final var headers = response.headers();
        headers.add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.TEXT_PLAIN);
        response.end(HELLO_WORLD_BUFFER, null);
    }
}