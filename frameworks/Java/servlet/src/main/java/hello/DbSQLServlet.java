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
 * Handles the single-query database test using a SQL database with a
 * Servlet-container managed pool.
 */
@SuppressWarnings("serial")
public class DbSQLServlet extends HttpServlet {
	// Database details.
	private static final String DB_QUERY = "SELECT * FROM World WHERE id = ?";
	// Database connection pool.
	@Resource(name = "jdbc/hello_world")
	private DataSource dataSource;

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException,
			IOException {
		World world = null;

		// Fetch some rows from the database.
		try (Connection conn = dataSource.getConnection()) {
			Common.modifySQLConnectionSettings(conn);
			try (PreparedStatement statement = conn.prepareStatement(DB_QUERY,
					ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
				statement.setInt(1, Common.getRandom());
				try (ResultSet results = statement.executeQuery()) {
					results.next(); // Here the expectation is ONLY only one
									// result row
					world = new World(results.getInt("id"), results.getInt("randomNumber"));
				}
			}
		} catch (SQLException sqlex) {
			throw new ServletException(sqlex);
		}

		// Set content type to JSON
		res.setHeader(Common.HEADER_CONTENT_TYPE, Common.CONTENT_TYPE_JSON);

		// Write JSON encoded message to the response.
		Common.MAPPER.writeValue(res.getOutputStream(), world);
	}
}