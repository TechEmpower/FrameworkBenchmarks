package hello;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.util.concurrent.*;

import javax.annotation.*;
import javax.servlet.*;
import javax.servlet.http.*;
import javax.sql.*;

import org.cache2k.Cache;
import org.cache2k.Cache2kBuilder;

/**
 * Cache
 */
@SuppressWarnings("serial")
public class Cache2kPostgresServlet extends HttpServlet {
	// Database details.
	private static final String DB_QUERY = "SELECT * FROM world";
	private static final int DB_ROWS = 10000;
	private static final int LIMIT = DB_ROWS + 1;
	
	// Database connection pool.
	@Resource(name = "jdbc/postgres_hello_world")
	private DataSource postgresDataSource;
	private Cache<Integer, CachedWorld> cache;

	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		// Fetch all rows from the database.
		final Map<Integer, CachedWorld> worlds = new HashMap<Integer, CachedWorld>();
		try (Connection conn = postgresDataSource.getConnection();
				PreparedStatement statement = conn.prepareStatement(DB_QUERY,
						ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
				ResultSet results = statement.executeQuery()) {
			while (results.next()) {
				CachedWorld some =  new CachedWorld(results.getInt("id"), results.getInt("randomNumber"));
				worlds.put(new Integer(some.getId()), some);
			}
		} catch (SQLException sqlex) {
			throw new ServletException(sqlex);
		}
		// Build the cache
		cache = new Cache2kBuilder<Integer, CachedWorld>() {}
		    .name("cachedWorld")
		    .eternal(true)
		    .entryCapacity(DB_ROWS)
		    .build();
		cache.putAll(worlds);
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException,
			IOException {
		final int count = Common.normalise(req.getParameter("queries"));
		final Random random = ThreadLocalRandom.current();

		List<Integer> keys = new ArrayList<Integer>();
		for (int i = 0; i < count; i++) {
			keys.add(new Integer(random.nextInt(LIMIT)));
		}
		
		// Set content type to JSON
		res.setHeader(Common.HEADER_CONTENT_TYPE, Common.CONTENT_TYPE_JSON);

		// Write JSON encoded message to the response.
		Common.MAPPER.writeValue(res.getOutputStream(), cache.getAll(keys).values());
	}
}