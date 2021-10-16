package net.officefloor.benchmark;

import java.net.Socket;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.impl.VertxBuilder;
import io.vertx.core.impl.VertxThread;
import io.vertx.core.spi.VertxThreadFactory;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgConnection;
import io.vertx.pgclient.impl.PgConnectionImpl;
import io.vertx.sqlclient.PropertyKind;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.Tuple;
import io.vertx.sqlclient.impl.QueryResultHandler;
import io.vertx.sqlclient.impl.RowDesc;
import io.vertx.sqlclient.impl.command.CommandScheduler;
import io.vertx.sqlclient.impl.command.ExtendedQueryCommand;
import net.officefloor.server.RequestHandler;
import net.officefloor.server.http.parse.HttpRequestParser;
import net.officefloor.vertx.OfficeFloorVertx;
import net.openhft.affinity.Affinity;

/**
 * R2DBC server.
 *
 * @author Daniel Sagenschneider
 */
public class RawSqlClientOfficeFloorMain implements DatabaseOperations {

	/**
	 * Run application.
	 */
	public static void main(String[] args) throws Throwable {
		RawWoof.run(args, (socketCount, server, port, database, username,
				password) -> new RawSqlClientOfficeFloorMain(socketCount, server, port, database, username, password));
	}

	/**
	 * Query executor.
	 */
	private static class QueryExecutor implements Promise<Boolean> {

		private final CommandScheduler scheduler;

		private QueryExecutor(CommandScheduler scheduler) {
			this.scheduler = scheduler;
		}

		public void execute(String sql, Tuple values, BiConsumer<List<Row>, Throwable> handler) {

			// Create the query
			ExtendedQueryCommand<List<Row>> query = ExtendedQueryCommand.createQuery(sql, null, values, true,
					Collectors.toList(), new QueryResultHandler<List<Row>>() {

						@Override
						public <V> void addProperty(PropertyKind<V> property, V value) {
							// Not required
						}

						@Override
						public void handleResult(int updatedCount, int size, RowDesc desc, List<Row> result,
								Throwable failure) {
							handler.accept(result, failure);
						}
					});

			// Execute query
			this.scheduler.schedule(query, this);
		}

		/*
		 * =============== Promise ==================
		 */

		@Override
		public boolean tryComplete(Boolean result) {
			return true;
		}

		@Override
		public boolean tryFail(Throwable cause) {
			return true;
		}

		@Override
		public Future<Boolean> future() {
			return null;
		}
	}

	/**
	 * {@link ThreadLocal} {@link QueryExecutor} instances.
	 */
	private final ThreadLocal<QueryExecutor> threadLocalQueryExecutor;

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
	public RawSqlClientOfficeFloorMain(int socketCount, String server, int port, String database, String username,
			String password) {

		// Should be all I/O processing for SQL responses
		System.setProperty("vertx.nettyIORatio", "100");

		// Create connection
		PgConnectOptions connectOptions = new PgConnectOptions().setHost(server).setPort(port).setDatabase(database)
				.setUser(username).setPassword(password).setCachePreparedStatements(true).setTcpNoDelay(true)
				.setTcpQuickAck(true);

		// Provide connection
		this.threadLocalQueryExecutor = new ThreadLocal<QueryExecutor>() {
			@Override
			protected QueryExecutor initialValue() {
				try {
					// Obtain thread affinity
					BitSet affinity = Affinity.getAffinity();
					System.out.println(Thread.currentThread().getName() + " has affinity " + affinity);

					// Setup Vertx for connection
					VertxOptions options = new VertxOptions().setPreferNativeTransport(true).setEventLoopPoolSize(1)
							.setWorkerPoolSize(1).setInternalBlockingPoolSize(1);
					VertxBuilder builder = new VertxBuilder(options).threadFactory(new VertxThreadFactory() {
						@Override
						public VertxThread newVertxThread(Runnable target, String name, boolean worker,
								long maxExecTime, TimeUnit maxExecTimeUnit) {
							return VertxThreadFactory.INSTANCE.newVertxThread(() -> {
								Affinity.setAffinity(affinity);
								target.run();
							}, name, worker, maxExecTime, maxExecTimeUnit);
						}
					});
					Vertx vertx = builder.init().vertx();

					// Obtain the connection
					PgConnection connection = OfficeFloorVertx.block(PgConnection.connect(vertx, connectOptions));
					PgConnectionImpl connectionImpl = (PgConnectionImpl) connection;

					// Return the query executor
					return new QueryExecutor(connectionImpl);
				} catch (Exception ex) {
					throw new IllegalStateException("Failed to setup connection", ex);
				}
			}
		};
	}

	/*
	 * ===================== DatabaseOperations ======================
	 */

	@Override
	public void threadSetup(RequestHandler<HttpRequestParser> requestHandler) {
		// Nothing thread specific to set up
	}

	@Override
	public void db(DbSendResponse sender) {
		threadLocalQueryExecutor.get().execute("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID=$1",
				Tuple.of(ThreadLocalRandom.current().nextInt(1, 10001)), (result, failure) -> {
					if (failure != null) {
						sender.sendError(failure);
						return;
					}
					Row row = result.get(0);
					sender.sendDb(new World(row.getInteger(0), row.getInteger(1)));
				});
	}

	@Override
	public void queries(int queryCount, QueriesSendResponse sender) {
		QueryExecutor executor = threadLocalQueryExecutor.get();
		World[] worlds = new World[queryCount];
		AtomicInteger index = new AtomicInteger();
		BiConsumer<List<Row>, Throwable> handler = (result, failure) -> {
			if (failure != null) {
				sender.sendError(failure);
				return;
			}
			int lastIndex = -1;
			for (Row row : result) {
				lastIndex = index.getAndIncrement();
				worlds[lastIndex] = new World(row.getInteger(0), row.getInteger(1));
			}
			if ((lastIndex + 1) >= queryCount) {
				sender.sendQueries(worlds);
			}
		};
		for (int i = 0; i < queryCount; i++) {
			executor.execute("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID=$1",
					Tuple.of(ThreadLocalRandom.current().nextInt(1, 10001)), handler);
		}
	}

	@Override
	public void fortunes(FortunesSendResponse sender) {
		threadLocalQueryExecutor.get().execute("SELECT ID, MESSAGE FROM FORTUNE", Tuple.tuple(), (result, failure) -> {
			if (failure != null) {
				sender.sendError(failure);
				return;
			}
			List<Fortune> fortunes = new ArrayList<>(16);
			for (Row row : result) {
				fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
			}
			sender.sendFortunes(fortunes);
		});
	}

	@Override
	public void update(int queryCount, UpdateSendResponse sender) {
		QueryExecutor executor = threadLocalQueryExecutor.get();
		World[] worlds = new World[queryCount];
		AtomicInteger updateIndex = new AtomicInteger();
		BiConsumer<List<Row>, Throwable> updateHandler = (result, failure) -> {
			if (failure != null) {
				sender.sendError(failure);
				return;
			}
			if (updateIndex.incrementAndGet() >= queryCount) {
				sender.sendUpdate(worlds);
			}
		};
		AtomicInteger selectIndex = new AtomicInteger();
		BiConsumer<List<Row>, Throwable> selectHandler = (result, failure) -> {
			if (failure != null) {
				sender.sendError(failure);
				return;
			}
			for (Row row : result) {
				World world = new World(row.getInteger(0), ThreadLocalRandom.current().nextInt(1, 10001));
				worlds[selectIndex.getAndIncrement()] = world;
				executor.execute("UPDATE world SET randomnumber=$1 WHERE id=$2", Tuple.of(world.randomNumber, world.id),
						updateHandler);
			}
		};
		for (int i = 0; i < queryCount; i++) {
			executor.execute("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID=$1",
					Tuple.of(ThreadLocalRandom.current().nextInt(1, 10001)), selectHandler);
		}
	}

	@Override
	public void cached(int updateCount, CachedSendResponse sender) {
		throw new UnsupportedOperationException("/cached-worlds test not supported");
	}

}
