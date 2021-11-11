package hello.controller;

import hello.domain.World;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ThreadLocalRandom;

import javax.sql.DataSource;

import org.restexpress.Request;
import org.restexpress.Response;

public class QueriesMysqlController {
	// Database details.
	private static final String DB_QUERY = "SELECT * FROM World WHERE id = ?";
	private static final int DB_ROWS = 10000;

	private DataSource mysqlDataSource;

	public QueriesMysqlController(DataSource dataSource) {
		super();
		this.mysqlDataSource = dataSource;
	}

	public Object read(Request request, Response response) throws SQLException {
		final int count = getQueryCount(request);
		final World[] worlds = new World[count];

		// Fetch some rows from the database.
		try (Connection conn = mysqlDataSource.getConnection()) {
			try (PreparedStatement statement = conn.prepareStatement(DB_QUERY,
					ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
				// Run the query the number of times requested.
				for (int i = 0; i < count; i++) {
					final int id = 1 + ThreadLocalRandom.current().nextInt(DB_ROWS);
					statement.setInt(1, id);

					try (ResultSet results = statement.executeQuery()) {
						results.next();
						worlds[i] = new World(results.getLong("id"), results.getInt("randomNumber"));
					}
				}
			}
		}

		return worlds;
	}

	private int getQueryCount(Request request) {
		String value = request.getHeader("queries");

		try {
			int parsedValue = Integer.parseInt(value);
			return Math.min(500, Math.max(1, parsedValue));
		} catch (NumberFormatException e) {
			return 1;
		}
	}
}
