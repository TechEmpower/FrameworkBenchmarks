
package io.helidon.benchmark.nima.models;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonBuilderFactory;

public interface DbRepository {

    JsonBuilderFactory JSON = Json.createBuilderFactory(Collections.emptyMap());

    default World getWorld() {
        return getWorld(randomWorldNumber());
    }

    World getWorld(int id);

    List<World> getWorlds(int count);

    default JsonArray getWorldsAsJson(int count) {
        JsonArrayBuilder result = JSON.createArrayBuilder();
        for (World world : getWorlds(count)) {
            result.add(world.toJson());
        }
        return result.build();
    }

    World updateWorld(World world);

    List<World> updateWorlds(int count);

    List<Fortune> getFortunes();

    static int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }
}