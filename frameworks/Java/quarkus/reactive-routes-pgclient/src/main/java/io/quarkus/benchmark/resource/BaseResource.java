package io.quarkus.benchmark.resource;

import io.netty.handler.codec.http.HttpHeaderValues;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.json.jackson.JacksonCodec;
import io.vertx.ext.web.RoutingContext;

public abstract class BaseResource {

    // TODO verify how to override/replace io.quarkus.vertx.runtime.jackson.QuarkusJacksonFactory in io.vertx.core.spi.JsonFactory
    private static final JacksonCodec JACKSON_CODEC = new JacksonCodec();

    void sendJson(final RoutingContext rc, final JsonObject json) {
        var response = rc.response();
        response.headers().add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.APPLICATION_JSON);
        response.end(JACKSON_CODEC.toBuffer(json, false), null);
    }

    void sendJson(final RoutingContext rc, final JsonArray json) {
        var response = rc.response();
        response.headers().add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.APPLICATION_JSON);
        response.end(JACKSON_CODEC.toBuffer(json, false), null);
    }

    Void handleFail(final RoutingContext rc, final Throwable t) {
        rc.response().setStatusCode(500).end(t.toString());
        return null;
    }

}