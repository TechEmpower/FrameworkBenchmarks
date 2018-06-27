package net.officefloor.benchmark;

import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import org.apache.commons.text.StringEscapeUtils;

import lombok.Data;
import net.officefloor.frame.api.function.FlowCallback;
import net.officefloor.plugin.managedfunction.clazz.FlowInterface;
import net.officefloor.plugin.section.clazz.Parameter;
import net.officefloor.plugin.section.clazz.Spawn;
import net.officefloor.server.http.HttpHeaderValue;
import net.officefloor.server.http.HttpResponse;
import net.officefloor.server.http.ServerHttpConnection;
import net.officefloor.server.stream.ServerWriter;
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

	public void update(@HttpQueryParameter("queries") String queries, UpdatesFlows flows) {
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
					flows.doUpdates(worlds);
				}
			});
		}
	}

	@FlowInterface
	public static interface UpdatesFlows {
		@Spawn
		void updateEntry(UpdateEntry entry, FlowCallback callback);

		void doUpdates(World[] worlds);
	}

	@Data
	public static class UpdateEntry {
		private final int id;
		private World world;
	}

	public void updateEntry(@Parameter UpdateEntry entry, Connection connection) throws SQLException {
		ThreadLocalRandom random = ThreadLocalRandom.current();
		try (PreparedStatement statement = connection.prepareStatement("SELECT ID FROM WORLD WHERE ID = ?",
				ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
			statement.setInt(1, entry.id);
			ResultSet resultSet = statement.executeQuery();
			resultSet.next();
			entry.world = new World(resultSet.getInt(1), random.nextInt(1, 10000));
		}
	}

	public void doUpdates(@Parameter World[] worlds, Connection connection, ObjectResponse<World[]> response)
			throws SQLException {
		Arrays.sort(worlds, (a, b) -> a.getId() - b.getId());
		try (PreparedStatement statement = connection
				.prepareStatement("UPDATE WORLD SET RANDOMNUMBER = ? WHERE ID = ?")) {
			for (int u = 0; u < worlds.length; u++) {
				statement.setInt(1, worlds[u].getRandomNumber());
				statement.setInt(2, worlds[u].getId());
				statement.addBatch();
			}
			statement.executeBatch();
		}
		response.send(worlds);
	}

	// =========== FORTUNES ==================

	private static final HttpHeaderValue TEXT_HTML = new HttpHeaderValue("text/html;charset=utf-8");

	private static final byte[] TEMPLATE_START = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
			.getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

	private static final byte[] FORTUNE_START = "<tr><td>".getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

	private static final byte[] FORTUNE_MIDDLE = "</td><td>".getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

	private static final byte[] FORTUNE_END = "</td></tr>".getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

	private static final byte[] TEMPLATE_END = "</table></body></html>"
			.getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

	public void fortunes(Connection connection, ServerHttpConnection httpConnection) throws IOException, SQLException {
		try (PreparedStatement statement = connection.prepareStatement("SELECT ID, MESSAGE FROM FORTUNE",
				ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
			List<Fortune> fortunes = new ArrayList<>();
			fortunes.add(new Fortune(0, "Additional fortune added at request time."));
			ResultSet resultSet = statement.executeQuery();
			while (resultSet.next()) {
				fortunes.add(new Fortune(resultSet.getInt(1), resultSet.getString(2)));
			}
			HttpResponse response = httpConnection.getResponse();
			response.setContentType(TEXT_HTML, null);
			ServerWriter writer = response.getEntityWriter();
			writer.write(TEMPLATE_START);
			Collections.sort(fortunes);
			for (int i = 0; i < fortunes.size(); i++) {
				Fortune fortune = fortunes.get(i);
				writer.write(FORTUNE_START);
				int id = fortune.getId();
				writer.write(Integer.valueOf(id).toString());
				writer.write(FORTUNE_MIDDLE);
				StringEscapeUtils.ESCAPE_HTML4.translate(fortune.getMessage(), writer);
				writer.write(FORTUNE_END);
			}
			writer.write(TEMPLATE_END);
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