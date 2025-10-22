package io.quarkus.benchmark.resource;

import io.netty.handler.codec.http.HttpHeaderValues;
import io.vertx.core.MultiMap;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpServerRequest;
import io.vertx.core.http.HttpServerResponse;
import jakarta.inject.Singleton;

@Singleton
public class PlaintextHttpHandler {
    private static final String HELLO_WORLD = "Hello, world!";
    private static final Buffer HELLO_WORLD_BUFFER = Buffer.buffer(HELLO_WORLD, "UTF-8");
    private static final CharSequence HELLO_WORLD_LENGTH = HttpHeaders.createOptimized("" + HELLO_WORLD.length());

    public void handle(final HttpServerRequest request) {
        final HttpServerResponse response = request.response();
        final MultiMap headers = response.headers();
        headers.add(HttpHeaders.CONTENT_LENGTH, HELLO_WORLD_LENGTH);
        headers.add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.TEXT_PLAIN);
        response.end(HELLO_WORLD_BUFFER, null);
    }
}
