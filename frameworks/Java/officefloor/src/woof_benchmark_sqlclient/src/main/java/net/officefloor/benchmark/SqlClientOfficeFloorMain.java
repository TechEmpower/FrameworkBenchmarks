package net.officefloor.benchmark;

import java.net.Socket;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.impl.VertxBuilder;
import io.vertx.core.impl.VertxThread;
import io.vertx.core.spi.VertxThreadFactory;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgConnection;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.SqlConnection;
import io.vertx.sqlclient.Tuple;
import net.officefloor.server.RequestHandler;
import net.officefloor.server.http.parse.HttpRequestParser;
import net.officefloor.vertx.OfficeFloorVertx;
import net.openhft.affinity.Affinity;

/**
 * R2DBC server.
 *
 * @author Daniel Sagenschneider
 */
public class SqlClientOfficeFloorMain implements DatabaseOperations {

	/**
	 * Run application.
	 */
	public static void main(String[] args) throws Throwable {
		RawWoof.run(args, (socketCount, server, port, database, username,
				password) -> new SqlClientOfficeFloorMain(socketCount, server, port, database, username, password));
	}

	/**
	 * Connections.
	 */
	private static class Connections {
		private int index = -1;
		private final PgConnection[] connections;

		private Connections(PgConnection[] connections) {
			this.connections = connections;
		}

		private PgConnection getConnection(int max) {
			this.index = (this.index + 1) % max;
			return this.connections[this.index];
		}
	}

	/**
	 * {@link ThreadLocal} {@link PgConnection} instances.
	 */
	private final ThreadLocal<Connections> threadLocalConnections;

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
	public SqlClientOfficeFloorMain(int socketCount, String server, int port, String database, String username,
			String password) {

		// Should be all I/O processing for SQL responses
		System.setProperty("vertx.nettyIORatio", "100");

		// Create connection
		PgConnectOptions connectOptions = new PgConnectOptions().setHost(server).setPort(port).setDatabase(database)
				.setUser(username).setPassword(password).setCachePreparedStatements(true).setTcpNoDelay(true)
				.setTcpQuickAck(true);

		// Provide connection
		this.threadLocalConnections = new ThreadLocal<Connections>() {
			@Override
			protected Connections initialValue() {
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
					PgConnection[] connections = new PgConnection[Math
							.max(Math.max(Math.max(DB_COUNT, QUERIES_COUNT), FORTUNES_COUNT), UPDATE_COUNT)];
					for (int i = 0; i < connections.length; i++) {
						connections[i] = OfficeFloorVertx.block(PgConnection.connect(vertx, connectOptions));
					}
					return new Connections(connections);
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

	private static final int DB_COUNT = 2;

	@Override
	public void db(DbSendResponse sender) {
		this.threadLocalConnections.get().getConnection(DB_COUNT)
				.preparedQuery("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID=$1")
				.execute(Tuple.of(ThreadLocalRandom.current().nextInt(1, 10001)), result -> {
					if (result.failed()) {
						sender.sendError(result.cause());
					} else {
						RowIterator<Row> rows = result.result().iterator();
						if (!rows.hasNext()) {
							sender.sendError(404);
						} else {
							Row row = rows.next();
							World world = new World(row.getInteger(0), row.getInteger(1));
							sender.sendDb(world);
						}
					}
				});
	}

	private static final int QUERIES_COUNT = 2;

	@Override
	public void queries(int queryCount, QueriesSendResponse sender) {
		World[] worlds = new World[queryCount];
		AtomicInteger count = new AtomicInteger(0);
		SqlConnection sqlConnection = this.threadLocalConnections.get().getConnection(QUERIES_COUNT);
		for (int i = 0; i < queryCount; i++) {
			sqlConnection.preparedQuery("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID=$1")
					.execute(Tuple.of(ThreadLocalRandom.current().nextInt(1, 10001)), result -> {
						if (result.failed()) {
							sender.sendError(result.cause());
						} else {
							RowIterator<Row> rows = result.result().iterator();
							if (!rows.hasNext()) {
								sender.sendError(404);
							} else {
								Row row = rows.next();
								int index = count.getAndIncrement();
								worlds[index] = new World(row.getInteger(0), row.getInteger(1));

								if ((index + 1) == queryCount) {
									sender.sendQueries(worlds);
								}
							}
						}
					});
		}
	}

	private static final int FORTUNES_COUNT = 1;

	@Override
	public void fortunes(FortunesSendResponse sender) {
		this.threadLocalConnections.get().getConnection(FORTUNES_COUNT).preparedQuery("SELECT ID, MESSAGE FROM FORTUNE")
				.execute(result -> {
					if (result.failed()) {
						sender.sendError(result.cause());
					} else {
						List<Fortune> fortunes = new ArrayList<>(16);
						RowIterator<Row> rows = result.result().iterator();
						while (rows.hasNext()) {
							Row row = rows.next();
							fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
						}
						sender.sendFortunes(fortunes);
					}
				});
	}

	private static final int UPDATE_COUNT = 2;

	@Override
	public void update(int queryCount, UpdateSendResponse sender) {
		World[] worlds = new World[queryCount];
		AtomicInteger count = new AtomicInteger(0);
		SqlConnection sqlConnection = this.threadLocalConnections.get().getConnection(UPDATE_COUNT);
		for (int i = 0; i < queryCount; i++) {
			sqlConnection.preparedQuery("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID=$1")
					.execute(Tuple.of(ThreadLocalRandom.current().nextInt(1, 10001)), result -> {
						if (result.failed()) {
							sender.sendError(result.cause());
						} else {
							RowIterator<Row> rows = result.result().iterator();
							if (!rows.hasNext()) {
								sender.sendError(404);
							} else {
								Row row = rows.next();

								// Ensure change to random number to trigger update
								int previousRandomNumber = row.getInteger(1);
								int newRandomNumber;
								do {
									newRandomNumber = ThreadLocalRandom.current().nextInt(1, 10001);
								} while (previousRandomNumber == newRandomNumber);
								int index = count.getAndIncrement();
								worlds[index] = new World(row.getInteger(0), newRandomNumber);

								if ((index + 1) == queryCount) {

									// All worlds obtained, so run update
									List<Tuple> batch = new ArrayList<>(queryCount);
									for (World update : worlds) {
										batch.add(Tuple.of(update.randomNumber, update.id));
									}

									// Sort to avoid deadlocks on updates
									Collections.sort(batch, (a, b) -> a.getInteger(1) - b.getInteger(1));

									sqlConnection.preparedQuery("UPDATE world SET randomnumber=$1 WHERE id=$2")
											.executeBatch(batch, ar -> {
												if (result.failed()) {
													sender.sendError(result.cause());
												} else {

													// Updated, so send response
													sender.sendUpdate(worlds);
												}
											});
								}
							}
						}
					});
		}
	}

	@Override
	public void cached(int updateCount, CachedSendResponse sender) {
		throw new UnsupportedOperationException("/cached-worlds test not supported");
	}

}
