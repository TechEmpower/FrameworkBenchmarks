package hello;

import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.jsontype.TypeSerializer;

/**
 * Some common functionality and constants used by the Servlet tests.
 */
public class Common {
	private static final int DB_ROWS = 10000;
	// Constants for setting the content type.
	protected static final String HEADER_CONTENT_TYPE = "Content-Type";
	protected static final String CONTENT_TYPE_JSON = "application/json";
	protected static final String CONTENT_TYPE_TEXT = "text/plain";
	protected static final String CONTENT_TYPE_HTML = "text/html";
	
	// Jackson encoder, reused for each response.
	protected static final ObjectMapper MAPPER = new ObjectMapper();

	private static final String DB_QUERY = "SELECT * FROM world";

	// Response message class.
	public static class HelloMessage {
		public final String message = "Hello, World!";
	}
	
	// Request parameter checking and normalisation
	public static int normalise(String param) {
		int count = 1;
		try {
			count = Integer.parseInt(param);
			// Bounds checks
			if (count > 500) {
				return 500;
			} else if (count < 1) {
				return 1;
			}
		} catch (NumberFormatException nfexc) {
		}
		return count;
	}
	
	// Preload all the data for the cache query test
	public static Map<Integer, CachedWorld> loadAll(Connection connection) throws SQLException{
		// Fetch all rows from the database.
		final Map<Integer, CachedWorld> worlds = new HashMap<Integer, CachedWorld>();
		try (Connection conn = connection;
				PreparedStatement statement = conn.prepareStatement(DB_QUERY,
						ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
				ResultSet results = statement.executeQuery()) {
			while (results.next()) {
				CachedWorld some =  new CachedWorld(results.getInt("id"), results.getInt("randomNumber"));
				worlds.put(new Integer(some.getId()), some);
			}
		}
		return worlds;
	}
	
	// Modify the SQL connection settings
	public static void modifySQLConnectionSettings(Connection connection) throws SQLException {
		connection.setAutoCommit(true);
	}
	
	public static int getRandom() {
		return 1 + ThreadLocalRandom.current().nextInt(DB_ROWS);
	}
}
