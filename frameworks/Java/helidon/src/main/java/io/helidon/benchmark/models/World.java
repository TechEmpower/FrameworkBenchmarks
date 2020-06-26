package io.helidon.benchmark.models;

import javax.json.Json;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObject;
import java.util.Collections;

public final class World {

    private static final JsonBuilderFactory JSON = Json.createBuilderFactory(Collections.emptyMap());

    public int id;
    public int randomNumber;

    public World(int id, int randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }

    public JsonObject toJson() {
        return JSON.createObjectBuilder().add("id", id).add("randomNumber", randomNumber).build();
    }
}
