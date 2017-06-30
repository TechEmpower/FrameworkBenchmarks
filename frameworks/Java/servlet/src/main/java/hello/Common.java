package hello;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.databind.*;

/**
 * Some common functionality and constants used by the Servlet tests.
 */
public class Common {
	// Constants for setting the content type.
	protected static final String HEADER_CONTENT_TYPE = "Content-Type";
	protected static final String CONTENT_TYPE_JSON = "application/json";
	protected static final String CONTENT_TYPE_TEXT = "text/plain";
	protected static final String CONTENT_TYPE_HTML = "text/html";

	// Jackson encoder, reused for each response.
	protected static final ObjectMapper MAPPER = new ObjectMapper();
	
	private static final String DB_QUERY = "SELECT * FROM world";
	
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
}