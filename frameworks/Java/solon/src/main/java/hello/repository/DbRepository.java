package hello.repository;

import hello.model.Fortune;
import hello.model.World;

import java.util.List;

public interface DbRepository {

    World getWorld(int id) throws Exception;

    void updateWorlds(List<World> worlds) throws Exception;

    List<Fortune> fortunes() throws Exception;
}
