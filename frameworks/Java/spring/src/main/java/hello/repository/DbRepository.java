package hello.repository;

import java.util.List;

import hello.model.Fortune;
import hello.model.World;

public interface DbRepository {

	World getWorld(int id);

	void updateWorlds(List<World> worlds);

	List<Fortune> fortunes();
}
