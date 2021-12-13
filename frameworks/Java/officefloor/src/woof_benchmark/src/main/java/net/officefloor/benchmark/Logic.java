package net.officefloor.benchmark;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;

import javax.persistence.EntityManager;

import lombok.Data;
import net.officefloor.cache.Cache;
import net.officefloor.web.HttpQueryParameter;
import net.officefloor.web.ObjectResponse;

/**
 * Logic.
 */
public class Logic {

	private static ThreadLocal<Set<Integer>> uniqueIdentifiers = new ThreadLocal<>() {
		@Override
		protected Set<Integer> initialValue() {
			return new HashSet<>(100);
		}
	};

	// =========== JSON ===================

	@Data
	public static class Message {
		private final String message;
	}

	public void json(ObjectResponse<Message> response) {
		response.send(new Message("Hello, World!"));
	}

	// ============ DB ====================

	public void db(EntityManager entityManager, ObjectResponse<World> response) {
		response.send(entityManager.find(World.class, ThreadLocalRandom.current().nextInt(1, 10001)));
	}

	// ========== QUERIES ==================

	public void queries(@HttpQueryParameter("queries") String queries, EntityManager entityManager,
			ObjectResponse<World[]> response) {
		int count = getQueryCount(queries);

		// Set up for unique numbers
		ThreadLocalRandom random = ThreadLocalRandom.current();
		Set<Integer> uniqueSet = uniqueIdentifiers.get();
		uniqueSet.clear();

		// Obtain the list of worlds
		World[] worlds = new World[count];
		for (int i = 0; i < worlds.length; i++) {

			// Obtain unique identifier
			int randomNumber;
			do {
				randomNumber = random.nextInt(1, 10001);
			} while (uniqueSet.contains(randomNumber));
			uniqueSet.add(randomNumber);

			// Obtain the world (unique id so always queries)
			worlds[i] = entityManager.find(World.class, randomNumber);
		}
		response.send(worlds);
	}

	// ========== CACHED ==================

	public void cached(@HttpQueryParameter("count") String queries, Cache<Integer, CachedWorld> cache,
			ObjectResponse<CachedWorld[]> response) {
		int count = getQueryCount(queries);

		// Set up for unique numbers
		ThreadLocalRandom random = ThreadLocalRandom.current();

		// Obtain the list of cached worlds
		CachedWorld[] worlds = new CachedWorld[count];
		for (int i = 0; i < worlds.length; i++) {

			// Obtain unique identifier
			int randomNumber = random.nextInt(1, 10001);

			// Obtain the cached world
			worlds[i] = cache.get(randomNumber);
		}
		response.send(worlds);
	}

	// =========== UPDATES ===================

	public void update(@HttpQueryParameter("queries") String queries, EntityManager entityManager,
			ObjectResponse<World[]> response) {
		int count = getQueryCount(queries);

		// Set up for unique numbers
		ThreadLocalRandom random = ThreadLocalRandom.current();
		Set<Integer> uniqueSet = uniqueIdentifiers.get();
		uniqueSet.clear();

		// Create list of worlds
		World[] worlds = new World[count];
		for (int i = 0; i < count; i++) {

			// Obtain unique identifier
			int randomNumber;
			do {
				randomNumber = random.nextInt(1, 10001);
			} while (uniqueSet.contains(randomNumber));
			uniqueSet.add(randomNumber);

			// Obtain the world
			World world = entityManager.find(World.class, randomNumber);
			worlds[i] = world;

			// Ensure change to different random number
			int existing = world.getRandomNumber();
			do {
				randomNumber = random.nextInt(1, 10001);
			} while (randomNumber == existing);

			// Update to different number to cause update
			world.setRandomNumber(randomNumber);
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