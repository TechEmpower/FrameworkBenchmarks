package io.helidon.benchmark.models;

import javax.json.Json;
import javax.json.JsonObject;

public final class World {
    public int id;
    public int randomNumber;

    public World(int id, int randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }

    public JsonObject toJson() {
        return Json.createObjectBuilder().add("id", id).add("randomNumber", randomNumber).build();
    }
}
