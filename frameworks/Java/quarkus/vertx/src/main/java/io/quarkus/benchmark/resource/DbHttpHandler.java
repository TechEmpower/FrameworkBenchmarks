package io.quarkus.benchmark.resource;

import io.netty.handler.codec.http.HttpHeaderValues;
import io.quarkus.benchmark.repository.WorldRepository;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpServerRequest;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

@Singleton
public class DbHttpHandler {

    @Inject
    WorldRepository worldRepository;

    // write handle
    public void handle(final HttpServerRequest request) {
        worldRepository.loadRandomJsonWorld(jsonWorld -> {
            if (jsonWorld.succeeded()) {
                var res = request.response();
                res.headers().add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.APPLICATION_JSON);
                res.end(jsonWorld.result().toBuffer(), null);
            } else {
                request.response().setStatusCode(500).end();
            }
        });
    }


}
