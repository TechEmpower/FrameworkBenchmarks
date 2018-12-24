package net.officefloor.benchmark;

import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;

import lombok.Data;
import net.officefloor.web.HttpQueryParameter;
import net.officefloor.web.ObjectResponse;

/**
 * Logic.
 */
public class Logic {

	// =========== JSON ===================

	@Data
	public static class Message {
		private final String message;
	}

	public void json(ObjectResponse<Message> response) {
		response.send(new Message("Hello, World!"));
	}

	// ============ DB ====================

	public void db(WorldRepository repository, ObjectResponse<World> response) {
		response.send(repository.findById(ThreadLocalRandom.current().nextInt(1, 10001)).get());
	}

	// ========== QUERIES ==================

	public void queries(@HttpQueryParameter("queries") String queries, WorldRepository repository,
			ObjectResponse<World[]> response) {
		ThreadLocalRandom random = ThreadLocalRandom.current();
		int count = getQueryCount(queries);
		World[] worlds = new World[count];
		for (int i = 0; i < worlds.length; i++) {
			worlds[i] = repository.findById(random.nextInt(1, 10001)).get();
		}
		response.send(worlds);
	}

	// =========== UPDATES ===================

	public void update(@HttpQueryParameter("queries") String queries, WorldRepository repository,
			ObjectResponse<World[]> response) {
		ThreadLocalRandom random = ThreadLocalRandom.current();
		int count = getQueryCount(queries);
		int[] ids = new int[count];
		for (int i = 0; i < ids.length; i++) {
			ids[i] = random.nextInt(1, 10001);
		}
		Arrays.sort(ids);
		World[] worlds = new World[count];
		for (int i = 0; i < worlds.length; i++) {
			worlds[i] = repository.findById(ids[i]).get();
			worlds[i].setRandomNumber(random.nextInt(1, 10001));
			repository.save(worlds[i]);
		}
		response.send(worlds);
	}

	// =========== helper ===================

	private static int getQueryCount(String queries) {
		try {
			int count = Integer.parseInt(queries);
			return (count < 1) ? 1 : (count > 500) ? 500 : count;
		} catch (NumberFormatException ex) {
			return 1;
		}
	}

}