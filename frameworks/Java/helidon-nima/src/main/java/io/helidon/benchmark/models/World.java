package io.helidon.benchmark.models;

import javax.json.Json;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObject;
import java.util.Collections;

public record World (int id, int randomNumber) {
    private static final String ID_KEY = "id";
    private static final String ID_RANDOM_NUMBER = "randomNumber";
    private static final JsonBuilderFactory JSON = Json.createBuilderFactory(Collections.emptyMap());

    public JsonObject toJson() {
        return JSON.createObjectBuilder().add(ID_KEY, id).add(ID_RANDOM_NUMBER, randomNumber).build();
    }
}
