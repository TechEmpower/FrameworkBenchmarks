package net.officefloor.benchmark;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Executor;
import java.util.concurrent.ThreadLocalRandom;

import io.netty.channel.unix.Socket;
import io.r2dbc.pool.PoolingConnectionFactoryProvider;
import io.r2dbc.postgresql.api.PostgresqlException;
import io.r2dbc.spi.Batch;
import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.ConnectionFactoryOptions;
import net.officefloor.cache.Cache;
import net.officefloor.cache.constant.ConstantCacheManagedObjectSource;
import net.officefloor.frame.api.managedobject.ManagedObject;
import net.officefloor.frame.util.ManagedObjectSourceStandAlone;
import net.officefloor.frame.util.ManagedObjectUserStandAlone;
import net.officefloor.plugin.managedobject.poll.StatePollContext;
import net.officefloor.server.RequestHandler;
import net.officefloor.server.http.parse.HttpRequestParser;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

/**
 * R2DBC server.
 * 
 * @author Daniel Sagenschneider
 */
public class R2dbcOfficeFloorMain implements DatabaseOperations {

	/**
	 * Database query load capacity to handle validation load.
	 */
	private static final int QUERY_LOAD_CAPACITY = 512 * (20 + 1); // update 20 selects then batch

	/**
	 * Buffer size of queries.
	 */
	private static final int QUERY_BUFFER_SIZE = 512;

	/**
	 * Run application.
	 */
	public static void main(String[] args) throws Throwable {

		// Increase the buffer size (note: too high and cause OOM issues)
		System.setProperty("reactor.bufferSize.small", String.valueOf(QUERY_BUFFER_SIZE));

		// Run the WoOF server
		RawWoof.run(args, (socketCount, server, port, database, username,
				password) -> new R2dbcOfficeFloorMain(socketCount, server, port, database, username, password));
	}

	/**
	 * {@link ThreadLocal} {@link RateLimit}.
	 */
	private final ThreadLocal<RateLimit> threadLocalRateLimit = new ThreadLocal<RateLimit>();

	/**
	 * {@link ThreadLocal} {@link Connection} instances.
	 */
	private final ThreadLocal<Connection[]> threadLocalConnections;

	/**
	 * {@link Cache} of {@link CachedWorld}.
	 */
	private final Cache<Integer, CachedWorld> cache;

	/**
	 * Instantiate.
	 * 
	 * @param socketCount Number of server {@link Socket} instances.
	 * @param server      Name of database server.
	 * @param port        Port of database.
	 * @param database    Name of database within server.
	 * @param username    Username.
	 * @param password    Password.
	 */
	@SuppressWarnings("unchecked")
	public R2dbcOfficeFloorMain(int socketCount, String server, int port, String database, String username,
			String password) throws Throwable {

		// Must have enough connection capacity for initial load (+1 for rounding)
		int requiredConnectionsPerSocket = (QUERY_LOAD_CAPACITY / (socketCount * QUERY_BUFFER_SIZE)) + 1;
		int connectionsPerSocket = Math.max(4, requiredConnectionsPerSocket);
		System.out.println("Using " + connectionsPerSocket + " connections per socket");

		// Determine the pool size for connections
		int connectionPoolSize = socketCount * connectionsPerSocket;

		// Build the connection pool
		ConnectionFactoryOptions factoryOptions = ConnectionFactoryOptions.builder()
				.option(ConnectionFactoryOptions.DRIVER, "pool").option(ConnectionFactoryOptions.PROTOCOL, "postgresql")
				.option(ConnectionFactoryOptions.HOST, server).option(ConnectionFactoryOptions.PORT, port)
				.option(ConnectionFactoryOptions.DATABASE, database).option(ConnectionFactoryOptions.USER, username)
				.option(ConnectionFactoryOptions.PASSWORD, password)
				.option(PoolingConnectionFactoryProvider.MAX_SIZE, connectionPoolSize).build();
		ConnectionFactory connectionFactory = ConnectionFactories.get(factoryOptions);

		// Create thread local connection
		this.threadLocalConnections = new ThreadLocal<Connection[]>() {
			@Override
			protected Connection[] initialValue() {
				Connection[] connections = new Connection[connectionsPerSocket];
				for (int i = 0; i < connections.length; i++) {
					connections[i] = Mono.from(connectionFactory.create()).block();
				}
				return connections;
			}
		};

		// Provide the cache
		ManagedObjectSourceStandAlone source = new ManagedObjectSourceStandAlone();
		source.registerInvokeProcessServicer(0, (processIndex, parameter, managedObject) -> {

			// Poll database for cached data
			StatePollContext<Map<Integer, CachedWorld>> pollContext = (StatePollContext<Map<Integer, CachedWorld>>) parameter;
			Map<Integer, CachedWorld> data = new HashMap<>();
			try {
				Flux.from(connectionFactory.create()).flatMap((connection) -> {
					return Flux.from(connection.createStatement("SELECT ID, RANDOMNUMBER FROM WORLD").execute())
							.flatMap(result -> Flux.from(result.map((row, metadata) -> {
								Integer id = row.get(0, Integer.class);
								Integer randomNumber = row.get(1, Integer.class);
								CachedWorld cachedWorld = new CachedWorld(id, randomNumber);
								data.put(id, cachedWorld);
								return cachedWorld;
							}))).last().flatMap(ignore -> Mono.from(connection.close()));
				}).blockLast();
				pollContext.setNextState(data, -1, null);
			} catch (Exception ex) {
			}
		});
		ManagedObject cacheMo = new ManagedObjectUserStandAlone()
				.sourceManagedObject(source.loadManagedObjectSource(ConstantCacheManagedObjectSource.class));
		this.cache = (Cache<Integer, CachedWorld>) cacheMo.getObject();
	}

	public void sendDatabaseError(Throwable failure, AbstractSendResponse response) {

		// Handle issue of prepared statement not found
		// (seems unsafe memory issue in R2DBC that occurs during start then stops)
		if (failure instanceof PostgresqlException) {
			PostgresqlException postgresqlException = (PostgresqlException) failure;
			if ("26000".equals(postgresqlException.getErrorDetails().getCode())) {
				// Prepared statement not existing
				response.sendError(503); // consider overloaded in connection setup during warm up
			}
		}

		// Just send the failure
		response.sendError(failure);
	}

	/*
	 * ===================== DatabaseOperations ======================
	 */

	@Override
	public void threadSetup(RequestHandler<HttpRequestParser> requestHandler) {

		// Ensure rate limits for socket servicing thread
		// Note: will always create before servicing any requests
		if (this.threadLocalRateLimit.get() == null) {
			Connection[] connections = this.threadLocalConnections.get();
			RateLimit rateLimit = new RateLimit(requestHandler, connections);
			this.threadLocalRateLimit.set(rateLimit);
		}
	}

	@Override
	public void db(DbSendResponse sender) {

		// Determine if will overload queries
		RateLimitedConnection conn = this.threadLocalRateLimit.get().getAvailableConnection(1);
		if (conn == null) {
			sender.sendOverloaded();
			return; // rate limited
		}

		// Service
		Mono.from(conn.connection.createStatement("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
				.bind(0, ThreadLocalRandom.current().nextInt(1, 10001)).execute())
				.flatMap(result -> Mono.from(result.map((row, metadata) -> {
					Integer id = row.get(0, Integer.class);
					Integer number = row.get(1, Integer.class);
					return new World(id, number);
				}))).publishOn(conn.writeScheduler).subscribe(world -> {
					sender.sendDb(world);
				}, error -> {
					this.sendDatabaseError(error, sender);
				}, () -> {
					conn.processed(1);
				});
	}

	@Override
	public void queries(int queryCount, QueriesSendResponse sender) {

		// Determine if will overload queries
		RateLimitedConnection conn = this.threadLocalRateLimit.get().getAvailableConnection(queryCount);
		if (conn == null) {
			sender.sendOverloaded();
			return; // rate limited
		}

		// Service
		Flux.range(1, queryCount)
				.flatMap(index -> conn.connection.createStatement("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
						.bind(0, ThreadLocalRandom.current().nextInt(1, 10001)).execute())
				.flatMap(result -> Flux.from(result.map((row, metadata) -> {
					Integer id = row.get(0, Integer.class);
					Integer number = row.get(1, Integer.class);
					return new World(id, number);
				}))).collectList().publishOn(conn.writeScheduler).subscribe(worlds -> {
					sender.sendQueries(worlds.toArray(World[]::new));
				}, error -> {
					this.sendDatabaseError(error, sender);
				}, () -> {
					conn.processed(queryCount);
				});
	}

	@Override
	public void fortunes(FortunesSendResponse sender) {

		// Determine if will overload queries
		RateLimitedConnection conn = this.threadLocalRateLimit.get().getAvailableConnection(1);
		if (conn == null) {
			sender.sendOverloaded();
			return; // rate limited
		}

		// Service
		Flux.from(conn.connection.createStatement("SELECT ID, MESSAGE FROM FORTUNE").execute())
				.flatMap(result -> Flux.from(result.map((row, metadata) -> {
					Integer id = row.get(0, Integer.class);
					String message = row.get(1, String.class);
					return new Fortune(id, message);
				}))).collectList().publishOn(conn.writeScheduler).subscribe(fortunes -> {
					sender.sendFortunes(fortunes);
				}, error -> {
					this.sendDatabaseError(error, sender);
				}, () -> {
					conn.processed(1);
				});
	}

	@Override
	public void update(int queryCount, UpdateSendResponse sender) {

		int executeQueryCount = queryCount + 1; // select all and update

		// Determine if will overload queries
		RateLimitedConnection conn = this.threadLocalRateLimit.get().getAvailableConnection(executeQueryCount);
		if (conn == null) {
			sender.sendOverloaded();
			return; // rate limited
		}

		// Service
		Flux.range(1, queryCount)
				.flatMap(index -> conn.connection.createStatement("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
						.bind(0, ThreadLocalRandom.current().nextInt(1, 10001)).execute())
				.flatMap(result -> Flux.from(result.map((row, metadata) -> {
					Integer id = row.get(0, Integer.class);
					Integer number = row.get(1, Integer.class);
					return new World(id, number);
				}))).collectList().flatMap(worlds -> {
					Collections.sort(worlds, (a, b) -> a.id - b.id);
					Batch batch = conn.connection.createBatch();
					for (World world : worlds) {

						// Ensure change to random number to trigger update
						int newRandomNumber;
						do {
							newRandomNumber = ThreadLocalRandom.current().nextInt(1, 10001);
						} while (world.randomNumber == newRandomNumber);
						world.randomNumber = newRandomNumber;

						batch.add("UPDATE WORLD SET RANDOMNUMBER = " + world.randomNumber + " WHERE ID = " + world.id);
					}
					return Mono.from(batch.execute()).map((result) -> worlds);
				}).publishOn(conn.writeScheduler).subscribe(worlds -> {
					sender.sendUpdate(worlds.toArray(World[]::new));
				}, error -> {
					this.sendDatabaseError(error, sender);
				}, () -> {
					conn.processed(executeQueryCount);
				});
	}

	@Override
	public void cached(int queryCount, CachedSendResponse sender) {

		// Set up for unique numbers
		ThreadLocalRandom random = ThreadLocalRandom.current();

		// Obtain the list of cached worlds
		CachedWorld[] cachedWorlds = new CachedWorld[queryCount];
		for (int i = 0; i < cachedWorlds.length; i++) {

			// Obtain unique identifier
			int randomNumber = random.nextInt(1, 10001);

			// Obtain the cached world
			cachedWorlds[i] = cache.get(randomNumber);
		}

		// Send cached worlds
		sender.sendCached(cachedWorlds);
	}

	private static class RateLimit {

		private final RateLimitedConnection[] rateLimitedConnections;

		private final Executor socketExecutor;

		private RateLimit(RequestHandler<HttpRequestParser> requestHandler, Connection[] connections) {

			// Create the write scheduler
			this.socketExecutor = (runnable) -> requestHandler.execute(() -> {
				runnable.run();
			});
			Scheduler writeScheduler = Schedulers.fromExecutor(this.socketExecutor);

			// Create the rate limited connections
			this.rateLimitedConnections = new RateLimitedConnection[connections.length];
			for (int i = 0; i < this.rateLimitedConnections.length; i++) {
				this.rateLimitedConnections[i] = new RateLimitedConnection(connections[i], writeScheduler);
			}
		}

		private RateLimitedConnection getAvailableConnection(int queryCount) {

			// Determine available connection for limit
			for (int i = 0; i < this.rateLimitedConnections.length; i++) {
				RateLimitedConnection connection = this.rateLimitedConnections[i];

				// Determine if query count reached
				int newCount = connection.activeQueries + queryCount;
				if (newCount <= QUERY_BUFFER_SIZE) {
					// Connection available for load
					connection.activeQueries = newCount;
					return connection;
				}
			}

			// As here, no available connection
			return null;
		}
	}

	private static class RateLimitedConnection {

		private final Scheduler writeScheduler;

		private final Connection connection;

		private int activeQueries;

		private RateLimitedConnection(Connection connection, Scheduler writeScheduler) {
			this.connection = connection;
			this.writeScheduler = writeScheduler;
		}

		private void processed(int queryCount) {

			// Update the active queries
			this.activeQueries -= queryCount;
		}
	}

}