package io.quarkus.benchmark.resource;

import io.netty.handler.codec.http.HttpHeaderValues;
import io.quarkus.benchmark.repository.WorldRepository;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpServerRequest;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import static io.quarkus.benchmark.resource.HttpQueryParameterUtils.queriesFrom;

@Singleton
public class UpdateHttpHandler {

    @Inject
    WorldRepository worldRepository;

    public void handle(final HttpServerRequest request) {
        worldRepository.updateNJsonWorlds(queriesFrom(request), jsonWorlds -> {
            if (jsonWorlds.succeeded()) {
                var res = request.response();
                res.headers().add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.APPLICATION_JSON);
                res.end(jsonWorlds.result().toBuffer(), null);
            } else {
                request.response().setStatusCode(500).end();
            }
        });
    }
}
