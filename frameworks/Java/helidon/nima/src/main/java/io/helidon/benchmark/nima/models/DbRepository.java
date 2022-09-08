
package io.helidon.benchmark.nima.models;

import java.util.List;

public interface DbRepository {

    World getWorld(int id);

    World updateWorld(World world);

    List<Fortune> getFortunes();
}