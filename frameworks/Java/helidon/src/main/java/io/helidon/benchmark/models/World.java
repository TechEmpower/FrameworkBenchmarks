package io.helidon.benchmark.models;

import javax.json.Json;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObject;
import java.util.Collections;

public final class World {

    private static final String ID_KEY = "id";
    private static final String ID_RANDOM_NUMBER = "randomNumber";
    private static final JsonBuilderFactory JSON = Json.createBuilderFactory(Collections.emptyMap());

    public int id;
    public int randomNumber;

    public World(int id, int randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }

    public JsonObject toJson() {
        return JSON.createObjectBuilder().add(ID_KEY, id).add(ID_RANDOM_NUMBER, randomNumber).build();
    }
}
