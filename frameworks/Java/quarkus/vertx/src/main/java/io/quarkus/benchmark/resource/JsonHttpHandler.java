package io.quarkus.benchmark.resource;

import io.netty.handler.codec.http.HttpHeaderValues;
import io.quarkus.benchmark.model.JsonMessage;
import io.vertx.core.MultiMap;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpServerRequest;
import io.vertx.core.http.HttpServerResponse;
import jakarta.inject.Singleton;

@Singleton
public class JsonHttpHandler {

    public void handle(final HttpServerRequest request) {
        final HttpServerResponse response = request.response();
        final MultiMap headers = response.headers();
        headers.add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.APPLICATION_JSON);
        response.end(new JsonMessage("Hello, World!").toBuffer(), null);
    }

}
