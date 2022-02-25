package io.quarkus.benchmark.resource;

import java.io.PrintWriter;
import java.io.StringWriter;

import javax.inject.Inject;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.vertx.ext.web.RoutingContext;

public abstract class BaseResource {
    
    @Inject
    ObjectMapper mapper;

    void sendJson(RoutingContext rc, Object value) {
        try {
            rc.response().putHeader("Content-Type", "application/json");
            rc.response().end(mapper.writeValueAsString(value));
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }   
    }

    Void handleFail(RoutingContext rc, Throwable t) {
        var sw = new StringWriter();
        t.printStackTrace(new PrintWriter(sw));
        rc.response().setStatusCode(500).end(sw.toString());
        return null;
    }

}
