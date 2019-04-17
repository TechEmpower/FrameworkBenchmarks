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

public class QueriesMysqlController
{
	// Database details.
	private static final String DB_QUERY = "SELECT * FROM World WHERE id = ?";
	private static final int DB_ROWS = 10000;

	private DataSource mysqlDataSource;

	public QueriesMysqlController(DataSource dataSource)
	{
		super();
		this.mysqlDataSource = dataSource;
	}

	public Object read(Request request, Response response)
	throws SQLException
	{
		int count = 1;
		try {
			count = Integer.parseInt(request.getHeader("queries"));
			// Bounds checks
			if (count > 500) {
				count = 500;
			} else if (count < 1) {
				count = 1;
			}
		} catch (NumberFormatException nfexc) {
			//do nothing
		}		
		final World[] worlds = new World[count];
		final int random = 1 + ThreadLocalRandom.current().nextInt(DB_ROWS);

		// Fetch some rows from the database.
		try (Connection conn = mysqlDataSource.getConnection()) {
			try (PreparedStatement statement = conn.prepareStatement(DB_QUERY,
					ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
				// Run the query the number of times requested.
				for (int i = 0; i < count; i++) {
					final int id = random;
					statement.setInt(1, id);

					try (ResultSet results = statement.executeQuery()) {
						results.next();
						worlds[i] = new World(new Long(id), results.getInt("randomNumber"));
					}
				}
			}
		}

		return worlds;
	}
}
