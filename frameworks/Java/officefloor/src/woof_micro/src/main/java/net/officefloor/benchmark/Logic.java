package net.officefloor.benchmark;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ThreadLocalRandom;

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

	public void db(Connection connection, ObjectResponse<World> response) throws SQLException {
		try (PreparedStatement statement = connection.prepareStatement(
				"SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = ?", ResultSet.TYPE_FORWARD_ONLY,
				ResultSet.CONCUR_READ_ONLY)) {
			statement.setInt(1, ThreadLocalRandom.current().nextInt(1, 10000));
			ResultSet resultSet = statement.executeQuery();
			resultSet.next();
			World world = new World(resultSet.getInt(1), resultSet.getInt(2));
			response.send(world);
		}
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

	public void getEntry(@Parameter GetEntry entry, Connection connection) throws SQLException {
		try (PreparedStatement statement = connection.prepareStatement(
				"SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = ?", ResultSet.TYPE_FORWARD_ONLY,
				ResultSet.CONCUR_READ_ONLY)) {
			statement.setInt(1, entry.id);
			ResultSet resultSet = statement.executeQuery();
			resultSet.next();
			entry.world = new World(resultSet.getInt(1), resultSet.getInt(2));
		}
	}

	// =========== UPDATES ===================

	public void update(@HttpQueryParameter("queries") String queries, UpdatesFlows flows, Connection connection,
			ObjectResponse<World[]> response) {
		ThreadLocalRandom random = ThreadLocalRandom.current();
		int[] loaded = new int[] { 0 };
		int count = getQueryCount(queries);
		World[] worlds = new World[count];
		for (int i = 0; i < worlds.length; i++) {
			int index = i;
			UpdateEntry entry = new UpdateEntry(random.nextInt(1, 10000));
			flows.updateEntry(entry, (escalation) -> {
				worlds[index] = entry.world;
				loaded[0]++;
				if (loaded[0] >= count) {

					// Update the worlds
					PreparedStatement statement = connection
							.prepareStatement("UPDATE WORLD SET RANDOMNUMBER = ? WHERE ID = ?");
					for (int u = 0; u < worlds.length; u++) {
						worlds[u].setRandomNumber(random.nextInt(1, 10000));
						statement.setInt(1, worlds[u].getRandomNumber());
						statement.setInt(2, worlds[u].getId());
						statement.addBatch();
					}
					statement.executeBatch();

					// Sent the response
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
		private final int id;
		private World world;
	}

	public void updateEntry(@Parameter UpdateEntry entry, Connection connection) throws SQLException {
		try (PreparedStatement statement = connection.prepareStatement(
				"SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = ?", ResultSet.TYPE_FORWARD_ONLY,
				ResultSet.CONCUR_READ_ONLY)) {
			statement.setInt(1, entry.id);
			ResultSet resultSet = statement.executeQuery();
			resultSet.next();
			entry.world = new World(resultSet.getInt(1), resultSet.getInt(2));
		}
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