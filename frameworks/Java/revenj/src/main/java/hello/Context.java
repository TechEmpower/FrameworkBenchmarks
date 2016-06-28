package hello;

import com.dslplatform.json.JsonWriter;
import dsl.Boot;
import dsl.FrameworkBench.World;
import dsl.FrameworkBench.repositories.WorldRepository;
import org.revenj.extensibility.Container;
import org.revenj.patterns.*;

import java.io.IOException;
import java.sql.*;
import java.util.*;
import java.util.concurrent.*;

class Context {
	private static final ServiceLocator locator;
	private static final String jdbcUrl;

	static {
		try {
			javax.naming.Context ctx = new javax.naming.InitialContext();
			jdbcUrl = (String) ctx.lookup("java:comp/env/revenj.jdbcUrl");
			Properties props = new Properties();
			props.setProperty("revenj.notifications.status", "disabled");
			locator = Boot.configure(jdbcUrl, props);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public final JsonWriter json;
	public final WorldRepository repository;
	public final Connection connection;
	private final ThreadLocalRandom random;
	public final RepositoryBulkReader bulkReader;
	public final World[] worlds = new World[512];
	public final Callable[] callables = new Callable[512];

	public Context() {
		try {
			Container ctx = locator.resolve(Container.class);
			this.connection = DriverManager.getConnection(jdbcUrl);
			connection.setAutoCommit(true);
			ctx.registerInstance(connection);
			this.json = new JsonWriter();
			this.random = ThreadLocalRandom.current();
			this.repository = ctx.resolve(WorldRepository.class);
			this.bulkReader = ctx.resolve(RepositoryBulkReader.class);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public int getRandom10k() {
		return random.nextInt(10000) + 1;
	}

	public void loadWorlds(final int count) throws IOException {
		bulkReader.reset();
		for (int i = 0; i < count; i++) {
			callables[i] = bulkReader.find(World.class, Integer.toString(getRandom10k()));
		}
		bulkReader.execute();
		try {
			for (int i = 0; i < count; i++) {
				worlds[i] = ((Optional<World>) callables[i].call()).get();
			}
		} catch (Exception e) {
			throw new IOException(e);
		}
	}
}
