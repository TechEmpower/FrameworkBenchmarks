package models;

import ratpack.exec.Promise;

import java.util.List;

public interface DbRepository {
    Promise<World> getWorld(int id);

    Promise<List<World>> getWorlds(int[] ids);

    Promise<List<World>> findAndUpdateWorlds(int[] ids, int[] randomNumbers);

    Promise<List<Fortune>> fortunes();
}