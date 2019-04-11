package hello.controller;

import hello.domain.World;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ThreadLocalRandom;

import javax.sql.DataSource;

import com.strategicgains.restexpress.Request;
import com.strategicgains.restexpress.Response;

public class DbMysqlController {
	// Database details.
	private static final String DB_QUERY = "SELECT * FROM World WHERE id = ?";
	private static final int DB_ROWS = 10000;

	private DataSource mysqlDataSource;

	public DbMysqlController(DataSource dataSource) {
		super();
		this.mysqlDataSource = dataSource;
	}

	public Object read(Request request, Response response) throws SQLException {
		final int random = 1 + ThreadLocalRandom.current().nextInt(DB_ROWS);

		World world = null;

		// Fetch some rows from the database.
		try (Connection conn = mysqlDataSource.getConnection()) {
			try (PreparedStatement statement = conn.prepareStatement(DB_QUERY,
					ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
				statement.setInt(1, random);
				try (ResultSet results = statement.executeQuery()) {
					results.next(); // Here the expectation is ONLY one
									// result row
					world = new World(results.getLong("id"), results.getInt("randomNumber"));
				}
			}
		}
		return world;
	}
}
