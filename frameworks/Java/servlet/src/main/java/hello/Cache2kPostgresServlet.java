package hello;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

import javax.annotation.Resource;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.sql.DataSource;

import org.cache2k.Cache;
import org.cache2k.Cache2kBuilder;
import org.cache2k.IntCache;

/**
 * Cache
 */
@SuppressWarnings("serial")
public class Cache2kPostgresServlet extends HttpServlet {
	// Database details.
	private static final int DB_ROWS = 10000;

	// Database connection pool.
	@Resource(name = "jdbc/hello_world")
	private DataSource dataSource;
	private IntCache<CachedWorld> cache;

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
		cache = new Cache2kBuilder<Integer, CachedWorld>() {
		}.name("cachedWorld").eternal(true).entryCapacity(DB_ROWS).buildForIntKey();
		cache.putAll(worlds);
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException,
			IOException {
		final int count = Common.normalise(req.getParameter("queries"));
		final CachedWorld[] worlds = new CachedWorld[count];

		for (int i = 0; i < count; i++) {
			worlds[i] = cache.peek(ThreadLocalRandom.current().nextInt(DB_ROWS) + 1);
		}

		// Set content type to JSON
		res.setHeader(Common.HEADER_CONTENT_TYPE, Common.CONTENT_TYPE_JSON);

		// Write JSON encoded message to the response.
		Common.MAPPER.writeValue(res.getOutputStream(), worlds);
	}
}
