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

import javax.sql.DataSource;

import org.apache.commons.text.StringEscapeUtils;

import lombok.Data;
import net.officefloor.cache.Cache;
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

	public void db(DataSource dataSource, ObjectResponse<World> response) throws SQLException {
		try (Connection connection = dataSource.getConnection()) {
			try (PreparedStatement statement = connection.prepareStatement(
					"SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = ?", ResultSet.TYPE_FORWARD_ONLY,
					ResultSet.CONCUR_READ_ONLY)) {
				statement.setInt(1, ThreadLocalRandom.current().nextInt(1, 10001));
				ResultSet resultSet = statement.executeQuery();
				resultSet.next();
				World world = new World(resultSet.getInt(1), resultSet.getInt(2));
				response.send(world);
			}
		}
	}

	// ========== QUERIES ==================

	public void queries(@HttpQueryParameter("queries") String queries, DataSource dataSource,
			ObjectResponse<World[]> response) throws SQLException {
		try (Connection connection = dataSource.getConnection()) {
			ThreadLocalRandom random = ThreadLocalRandom.current();
			int count = getQueryCount(queries);
			World[] worlds = new World[count];
			try (PreparedStatement statement = connection.prepareStatement(
					"SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = ?", ResultSet.TYPE_FORWARD_ONLY,
					ResultSet.CONCUR_READ_ONLY)) {
				for (int i = 0; i < worlds.length; i++) {
					statement.setInt(1, random.nextInt(1, 10001));
					ResultSet resultSet = statement.executeQuery();
					resultSet.next();
					worlds[i] = new World(resultSet.getInt(1), resultSet.getInt(2));
				}
				response.send(worlds);
			}
		}
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

	public void update(@HttpQueryParameter("queries") String queries, DataSource dataSource,
			ObjectResponse<World[]> response) throws SQLException {
		try (Connection connection = dataSource.getConnection()) {
			ThreadLocalRandom random = ThreadLocalRandom.current();
			int count = getQueryCount(queries);
			World[] worlds = new World[count];
			try (PreparedStatement statement = connection.prepareStatement("SELECT ID FROM WORLD WHERE ID = ?",
					ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
				for (int i = 0; i < worlds.length; i++) {
					statement.setInt(1, random.nextInt(1, 10001));
					ResultSet resultSet = statement.executeQuery();
					resultSet.next();
					worlds[i] = new World(resultSet.getInt(1), random.nextInt(1, 10001));
				}
			}
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

	public void fortunes(DataSource dataSource, ServerHttpConnection httpConnection) throws IOException, SQLException {
		try (Connection connection = dataSource.getConnection()) {
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