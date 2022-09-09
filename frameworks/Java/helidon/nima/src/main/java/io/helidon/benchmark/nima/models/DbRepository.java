
package io.helidon.benchmark.nima.models;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public interface DbRepository {

    default World getWorld() {
        return getWorld(randomWorldNumber());
    }

    World getWorld(int id);

    List<World> getWorlds(int count);

    World updateWorld(World world);

    List<World> updateWorlds(int count);

    List<Fortune> getFortunes();

    static int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }
}