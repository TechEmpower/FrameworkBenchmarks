package io.quarkus.benchmark.model;

import io.vertx.core.json.JsonObject;

import java.util.Map;

public class JsonMessage extends JsonObject {

    public JsonMessage(final String message) {
        super(Map.of("message", message));
    }
}
