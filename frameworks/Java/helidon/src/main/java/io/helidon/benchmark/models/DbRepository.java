package io.helidon.benchmark.models;

import io.reactivex.Single;

import java.util.List;

public interface DbRepository {
    Single<World> getWorld(int id);

    Single<World> updateWorld(World world);

    Single<List<Fortune>> getFortunes();
}