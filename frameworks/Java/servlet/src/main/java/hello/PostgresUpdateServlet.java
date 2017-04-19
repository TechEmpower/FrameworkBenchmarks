package hello;

import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import javax.annotation.Resource;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.sql.DataSource;

/**
 * Database connectivity (with a Servlet-container managed pool) test.
 */
@SuppressWarnings("serial")
public class PostgresUpdateServlet extends HttpServlet {
	// Database details.
	private static final String DB_QUERY = "SELECT * FROM World WHERE id = ?";
	private static final String UPDATE_QUERY = "UPDATE World SET randomNumber = ? WHERE id = ?";
	private static final int DB_ROWS = 10000;

	// Database connection pool.
	@Resource(name = "jdbc/postgres_hello_world")
	private DataSource postgresDataSource;

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException,
			IOException {
		// Set content type to JSON
		res.setHeader(Common.HEADER_CONTENT_TYPE, Common.CONTENT_TYPE_JSON);

		// Reference the data source.
		final DataSource source = postgresDataSource;

		// Get the count of queries to run.
		int count = 1;
		try {
			count = Integer.parseInt(req.getParameter("queries"));

			// Bounds check.
			if (count > 500) {
				count = 500;
			}
			if (count < 1) {
				count = 1;
			}
		} catch (NumberFormatException nfexc) {
			// Do nothing.
		}

		// Fetch some rows from the database.
		final World[] worlds = new World[count];
		final Random random = ThreadLocalRandom.current();

		try (Connection conn = source.getConnection()) {
			try (PreparedStatement statement = conn.prepareStatement(DB_QUERY,
					ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
					PreparedStatement statement2 = conn.prepareStatement(UPDATE_QUERY)) {
				// Run the query the number of times requested.
				for (int i = 0; i < count; i++) {
					final int id = random.nextInt(DB_ROWS) + 1;
					statement.setInt(1, id);

					try (ResultSet results = statement.executeQuery()) {
						if (results.next()) {
							worlds[i] = new World(id, results.getInt("randomNumber"));

							// Update row
							worlds[i].setRandomNumber(random.nextInt(DB_ROWS) + 1);
							statement2.setInt(1, worlds[i].getRandomNumber());
							statement2.setInt(2, id);

							// Execute the update statement
							statement2.execute();

							/* 
							*  Applying batch updates will lead to transaction deadlocks.
							*  This could not be apparent on local testing but will be
							*  visible on higher concurrencies in the TFB test  environment.
							*/
						}
					}
				}
			}
		} catch (SQLException sqlex) {
			System.err.println("SQL Exception: " + sqlex);
		}

		// Write JSON encoded message to the response.
		try {
			Common.MAPPER.writeValue(res.getOutputStream(), worlds);
		} catch (IOException ioe) {
			// do nothing
		}
	}
}
