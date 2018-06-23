package net.officefloor.benchmark;

import java.util.concurrent.ThreadLocalRandom;

import javax.persistence.EntityManager;

import lombok.Data;
import net.officefloor.frame.api.function.FlowCallback;
import net.officefloor.plugin.managedfunction.clazz.FlowInterface;
import net.officefloor.plugin.section.clazz.Parameter;
import net.officefloor.plugin.section.clazz.Spawn;
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

	public void db(EntityManager entityManager, ObjectResponse<World> response) {
		response.send(entityManager.find(World.class, ThreadLocalRandom.current().nextInt(1, 10000)));
	}

	// ========== QUERIES ==================

	public void queries(@HttpQueryParameter("queries") String queries, QueriesFlows flows,
			ObjectResponse<World[]> response) {
		ThreadLocalRandom random = ThreadLocalRandom.current();
		int[] loaded = new int[] { 0 };
		int count = getQueryCount(queries);
		World[] worlds = new World[count];
		for (int i = 0; i < worlds.length; i++) {
			int index = i;
			GetEntry entry = new GetEntry(random.nextInt(1, 10000));
			flows.getEntry(entry, (escalation) -> {
				worlds[index] = entry.world;
				loaded[0]++;
				if (loaded[0] >= count) {
					response.send(worlds);
				}
			});
		}
	}

	@Data
	public static class GetEntry {
		private final int id;
		private World world;
	}

	@FlowInterface
	public static interface QueriesFlows {
		@Spawn
		void getEntry(GetEntry entry, FlowCallback callback);
	}

	public void getEntry(@Parameter GetEntry entry, EntityManager entityManager) {
		entry.world = entityManager.find(World.class, entry.id);
	}

	// =========== UPDATES ===================

	public void update(@HttpQueryParameter("queries") String queries, UpdatesFlows flows,
			ObjectResponse<World[]> response) {
		int[] loaded = new int[] { 0 };
		int count = getQueryCount(queries);
		World[] worlds = new World[count];
		for (int i = 0; i < worlds.length; i++) {
			int index = i;
			UpdateEntry entry = new UpdateEntry();
			flows.updateEntry(entry, (escalation) -> {
				worlds[index] = entry.world;
				loaded[0]++;
				if (loaded[0] >= count) {
					response.send(worlds);
				}
			});
		}
	}

	@FlowInterface
	public static interface UpdatesFlows {
		@Spawn
		void updateEntry(UpdateEntry entry, FlowCallback callback);
	}

	@Data
	public static class UpdateEntry {
		private World world;
	}

	public void updateEntry(@Parameter UpdateEntry entry, EntityManager entityManager) {
		ThreadLocalRandom random = ThreadLocalRandom.current();
		entry.world = entityManager.find(World.class, random.nextInt(1, 10000));
		entry.world.setRandomNumber(random.nextInt(1, 10000));
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