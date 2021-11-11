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
public class UpdateServlet extends HttpServlet {
	private static final String PARAMETER_QUERIES = "queries";
	// Database details.
	private static final String DB_QUERY = "SELECT * FROM World WHERE id = ?";
	private static final String UPDATE_QUERY = "UPDATE World SET randomNumber = ? WHERE id = ?";

	// Database connection pool.
	@Resource(name = "jdbc/hello_world")
	private DataSource mysqlDataSource;

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException,
			IOException {
		// Reference the data source.
		final DataSource source = mysqlDataSource;
		final int count = Common.normalise(req.getParameter(PARAMETER_QUERIES));
		final World[] worlds = new World[count];

		try (Connection conn = source.getConnection();
				PreparedStatement statement = conn.prepareStatement(DB_QUERY,
						ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
				PreparedStatement statement2 = conn.prepareStatement(UPDATE_QUERY,
						ResultSet.TYPE_FORWARD_ONLY)) {
			Common.modifySQLConnectionSettings(conn);
			// Run the query the number of times requested.
			for (int i = 0; i < count; i++) {
				final int id = Common.getRandom();
				statement.setInt(1, id);

				try (ResultSet results = statement.executeQuery()) {
					if (results.next()) {
						worlds[i] = new World(id, results.getInt("randomNumber"));

						// Update row
						worlds[i].setRandomNumber(Common.getRandom());
						statement2.setInt(1, worlds[i].getRandomNumber());
						statement2.setInt(2, id);

						// Add update statement to batch update
						statement2.addBatch();
					}
				}
				// Execute batch update
				statement2.executeBatch();
			}
		} catch (SQLException sqlex) {
			throw new ServletException(sqlex);
		}
		// Set content type to JSON
		res.setHeader(Common.HEADER_CONTENT_TYPE, Common.CONTENT_TYPE_JSON);
		// Write JSON encoded message to the response.
		Common.MAPPER.writeValue(res.getOutputStream(), worlds);
	}
}