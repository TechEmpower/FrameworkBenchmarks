package hello.repository;

import java.util.List;

import hello.model.Fortune;
import hello.model.World;

public interface DbRepository {
	World getWorld(int id);

	World updateWorld(World world, int randomNumber);

	List<Fortune> fortunes();
}
