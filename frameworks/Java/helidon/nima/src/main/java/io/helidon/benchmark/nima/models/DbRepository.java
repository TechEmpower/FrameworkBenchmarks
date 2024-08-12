
package io.helidon.benchmark.nima.models;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import jakarta.json.Json;
import jakarta.json.JsonBuilderFactory;

public interface DbRepository {

    JsonBuilderFactory JSON = Json.createBuilderFactory(Collections.emptyMap());

    World getWorld(int id);

    List<World> getWorlds(int count);

    World updateWorld(World world);

    List<World> updateWorlds(int count);

    List<Fortune> getFortunes();

    static int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }
}