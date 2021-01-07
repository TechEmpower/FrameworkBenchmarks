package io.quarkus.benchmark.resource;

import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.Json;
import io.vertx.ext.web.RoutingContext;

public abstract class BaseResource {

    void sendJson(RoutingContext rc, Object value) {
        rc.response()
                .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                .end(Json.encodeToBuffer(value));
    }

    Void handleFail(RoutingContext rc, Throwable t) {
        rc.response().setStatusCode(500).end(t.toString());
        return null;
    }

}
