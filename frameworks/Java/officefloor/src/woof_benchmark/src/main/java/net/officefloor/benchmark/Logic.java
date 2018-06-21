package net.officefloor.benchmark;

import java.util.concurrent.ThreadLocalRandom;

import javax.persistence.EntityManager;

import lombok.Data;
import net.officefloor.web.HttpQueryParameter;
import net.officefloor.web.ObjectResponse;

/**
 * Logic.
 */
public class Logic {

	@Data
	public static class Message {
		private final String message;
	}

	public void json(ObjectResponse<Message> response) {
		response.send(new Message("Hello, World!"));
	}

	public void db(EntityManager entityManager, ObjectResponse<World> response) {
		response.send(entityManager.find(World.class, ThreadLocalRandom.current().nextInt(1, 10000)));
	}

	public void queries(@HttpQueryParameter("queries") String queries, EntityManager entityManager,
			ObjectResponse<World[]> response) {
		ThreadLocalRandom random = ThreadLocalRandom.current();
		int count = getQueryCount(queries);
		World[] worlds = new World[count];
		for (int i = 0; i < worlds.length; i++) {
			worlds[i] = entityManager.find(World.class, random.nextInt(1, 10000));
		}
		response.send(worlds);
	}

	public void updates(@HttpQueryParameter("queries") String queries, EntityManager entityManager,
			ObjectResponse<World[]> response) {
		ThreadLocalRandom random = ThreadLocalRandom.current();
		int count = getQueryCount(queries);
		World[] worlds = new World[count];
		for (int i = 0; i < worlds.length; i++) {
			worlds[i] = entityManager.find(World.class, random.nextInt(1, 10000));
			worlds[i].setRandomNumber(random.nextInt(1, 10000));
		}
		response.send(worlds);
	}

	private static int getQueryCount(String queries) {
		int count = Integer.parseInt(queries);
		return (count < 1) ? 1 : (count > 500) ? 500 : count;
	}

}