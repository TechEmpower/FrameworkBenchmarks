package io.helidon.benchmark.models;

import io.helidon.common.reactive.Multi;
import io.helidon.common.reactive.Single;

public interface DbRepository {
    Single<World> getWorld(int id);

    Single<World> updateWorld(World world);

    Multi<Fortune> getFortunes();
}
