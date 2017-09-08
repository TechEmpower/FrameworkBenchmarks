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
	private static final int DB_ROWS = 10000;
	private static final int LIMIT = DB_ROWS + 1;
	
	// Database connection pool.
	@Resource(name = "jdbc/hello_world")
	private DataSource dataSource;
	private Cache<Integer, CachedWorld> cache;

	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		Map<Integer, CachedWorld> worlds;
		try {
			worlds = Common.loadAll(dataSource.getConnection());
		} catch (SQLException e) {
			throw new ServletException(e);
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

		//TODO prevent duplicate numbers to be added
		List<Integer> keys = new ArrayList<Integer>(count);
		for (int i = 0; i < count; i++) {
			keys.add(new Integer(random.nextInt(LIMIT)));
		}
		
		// Set content type to JSON
		res.setHeader(Common.HEADER_CONTENT_TYPE, Common.CONTENT_TYPE_JSON);

		// Write JSON encoded message to the response.
		Common.MAPPER.writeValue(res.getOutputStream(), cache.getAll(keys).values());
	}
}