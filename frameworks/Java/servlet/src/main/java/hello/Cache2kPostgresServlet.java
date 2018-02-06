package hello;

import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import javax.annotation.Resource;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.sql.DataSource;

import org.cache2k.Cache;
import org.cache2k.Cache2kBuilder;

/**
 * Cache
 */
@SuppressWarnings("serial")
public class Cache2kPostgresServlet extends HttpServlet {
	// Database details.
	private static final int DB_ROWS = 10000;
	private CircularList<Integer> randomNumbers;

	// Database connection pool.
	@Resource(name = "jdbc/hello_world")
	private DataSource dataSource;
	private Cache<Integer, CachedWorld> cache;

	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		randomNumbers = new CircularList<>(shuffledNumbers(DB_ROWS));

		Map<Integer, CachedWorld> worlds;
		try {
			worlds = Common.loadAll(dataSource.getConnection());
		} catch (SQLException e) {
			throw new ServletException(e);
		}

		// Build the cache
		cache = new Cache2kBuilder<Integer, CachedWorld>() {
		}.name("cachedWorld").eternal(true).entryCapacity(DB_ROWS).build();
		cache.putAll(worlds);
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException,
			IOException {
		final int count = Common.normalise(req.getParameter("queries"));
		final int start = ThreadLocalRandom.current().nextInt(DB_ROWS);
		List<Integer> keys = IntStream.range(start, start + count)
				.mapToObj(i -> randomNumbers.get(i)).collect(Collectors.toList());

		// Set content type to JSON
		res.setHeader(Common.HEADER_CONTENT_TYPE, Common.CONTENT_TYPE_JSON);

		// Write JSON encoded message to the response.
		Common.MAPPER.writeValue(res.getOutputStream(), cache.getAll(keys).values());
	}

	private List<Integer> shuffledNumbers(int size) {
		List<Integer> buffer = IntStream.rangeClosed(1, size).boxed().collect(Collectors.toList());
		Collections.shuffle(buffer, ThreadLocalRandom.current());
		return buffer;
	}

	// Credits here: https://stackoverflow.com/questions/18659792/circular-arraylist-extending-arraylist#18659966
	private class CircularList<E> extends ArrayList<E> {
		public CircularList(Collection<? extends E> arg0) {
			super(arg0);
		}

		@Override
		public E get(int index) {
			return super.get(index % size());
		}
	}
}
