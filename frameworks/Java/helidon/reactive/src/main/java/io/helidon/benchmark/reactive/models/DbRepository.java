package io.helidon.benchmark.reactive.models;

import java.util.List;

import io.helidon.common.reactive.Single;

public interface DbRepository {

    Single<World> getWorld(int id);

    Single<World> updateWorld(World world);

    Single<List<Fortune>> getFortunes();
}