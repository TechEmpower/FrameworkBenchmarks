/*
 * OfficeFloor - http://www.officefloor.net
 * Copyright (C) 2005-2018 Daniel Sagenschneider
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.officefloor.benchmark;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.nio.ByteBuffer;
import java.nio.channels.CancelledKeyException;
import java.nio.channels.ClosedChannelException;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.apache.commons.text.StringEscapeUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;

import io.r2dbc.pool.PoolingConnectionFactoryProvider;
import io.r2dbc.spi.Batch;
import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.ConnectionFactoryOptions;
import io.r2dbc.spi.R2dbcTransientResourceException;
import lombok.AllArgsConstructor;
import lombok.Data;
import net.officefloor.benchmark.R2dbcOfficeFloorMain.Fortune;
import net.officefloor.benchmark.R2dbcOfficeFloorMain.Message;
import net.officefloor.benchmark.R2dbcOfficeFloorMain.World;
import net.officefloor.frame.api.manage.OfficeFloor;
import net.officefloor.frame.api.manage.ProcessManager;
import net.officefloor.frame.api.managedobject.ManagedObjectContext;
import net.officefloor.frame.api.managedobject.ProcessSafeOperation;
import net.officefloor.frame.api.managedobject.pool.ThreadCompletionListener;
import net.officefloor.server.RequestHandler;
import net.officefloor.server.SocketManager;
import net.officefloor.server.SocketServicer;
import net.officefloor.server.http.AbstractHttpServicerFactory;
import net.officefloor.server.http.HttpHeaderName;
import net.officefloor.server.http.HttpHeaderValue;
import net.officefloor.server.http.HttpRequest;
import net.officefloor.server.http.HttpResponse;
import net.officefloor.server.http.HttpResponseHeaders;
import net.officefloor.server.http.HttpServerLocation;
import net.officefloor.server.http.HttpServerSocketManagedObjectSource;
import net.officefloor.server.http.HttpStatus;
import net.officefloor.server.http.ServerHttpConnection;
import net.officefloor.server.http.impl.HttpServerLocationImpl;
import net.officefloor.server.http.impl.ProcessAwareServerHttpConnectionManagedObject;
import net.officefloor.server.http.parse.HttpRequestParser;
import net.officefloor.server.http.parse.HttpRequestParser.HttpRequestParserMetaData;
import net.officefloor.server.stream.ServerWriter;
import net.officefloor.server.stream.impl.ThreadLocalStreamBufferPool;
import net.officefloor.web.executive.CpuCore;
import net.officefloor.web.executive.CpuCore.LogicalCpu;
import net.openhft.affinity.Affinity;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import net.officefloor.web.executive.CpuCore;
import net.openhft.affinity.Affinity;

/**
 * <p>
 * {@link SocketManager} raw performance.
 * <p>
 * Allows determining the overhead of the {@link OfficeFloor} framework.
 */
public class R2dbcOfficeFloorMain {

	/**
	 * Database query load capacity to handle validation load.
	 */
	private static final int QUERY_LOAD_CAPACITY = 512 * (20 + 1); // update 20 selects then batch

	/**
	 * Buffer size of queries.
	 */
	private static final int QUERY_BUFFER_SIZE = 512;

	/**
	 * {@link SocketManager}.
	 */
	public static SocketManager socketManager = null;

	/**
	 * {@link Logger}.
	 */
	private static Logger logger = Logger.getLogger(R2dbcOfficeFloorMain.class.getName());

	/**
	 * Run application.
	 */
	public static void main(String[] args) throws Exception {

		// Obtain the port from properties
		int port = args.length > 0 ? Integer.parseInt(args[0]) : 8080;

		// Ensure previous socket manager shutdown (typically from tests)
		if (socketManager != null) {
			socketManager.shutdown();
		}

		// Indicate details
		String server = System.getProperty("OFFICE.net_officefloor_jdbc_DataSourceManagedObjectSource.server",
				"tfb-database");
		System.out.println("Starting server on port " + port + " talking to database " + server);

		// Increase the buffer size (note: too high and cause OOM issues)
		System.setProperty("reactor.bufferSize.small", String.valueOf(QUERY_BUFFER_SIZE));

		// Create the server location
		HttpServerLocation serverLocation = new HttpServerLocationImpl("localhost", port, -1);

		// Create a thread factory per logical CPU
		ThreadCompletionListener[] threadCompletionListenerCapture = new ThreadCompletionListener[] { null };
		List<ThreadFactory> threadFactories = new LinkedList<>();
		for (CpuCore cpuCore : CpuCore.getCores()) {
			for (CpuCore.LogicalCpu logicalCpu : cpuCore.getCpus()) {

				// Create thread factory for logical CPU
				ThreadFactory boundThreadFactory = (runnable) -> new Thread(() -> {
					ThreadLocalStreamBufferPool bufferPool = (ThreadLocalStreamBufferPool) threadCompletionListenerCapture[0];
					try {
						// Bind thread to logical CPU
						Affinity.setAffinity(logicalCpu.getCpuAffinity());

						// Set up for thread local buffer pooling
						bufferPool.activeThreadLocalPooling();

						// Run logic for thread
						runnable.run();
					} finally {
						bufferPool.threadComplete();
					}
				});

				// Add the thread factory
				threadFactories.add(boundThreadFactory);
			}
		}
		ThreadFactory[] executionStrategy = threadFactories.toArray(new ThreadFactory[0]);
		System.out.println("Using " + executionStrategy.length + " executors");

		// Create the socket manager
		socketManager = HttpServerSocketManagedObjectSource.createSocketManager(executionStrategy,
				(threadCompletionListener) -> threadCompletionListenerCapture[0] = threadCompletionListener);

		// Must have enough connection capacity for initial load (+1 for rounding)
		int requiredConnectionsPerSocket = (QUERY_LOAD_CAPACITY / (executionStrategy.length * QUERY_BUFFER_SIZE)) + 1;
		int connectionsPerSocket = Math.max(4, requiredConnectionsPerSocket);
		System.out.println("Using " + connectionsPerSocket + " connections per socket");

		// Determine the pool size for connections
		int connectionPoolSize = executionStrategy.length * connectionsPerSocket;

		// Build the connection pool
		ConnectionFactoryOptions factoryOptions = ConnectionFactoryOptions.builder()
				.option(ConnectionFactoryOptions.DRIVER, "pool").option(ConnectionFactoryOptions.PROTOCOL, "postgresql")
				.option(ConnectionFactoryOptions.HOST, server).option(ConnectionFactoryOptions.PORT, 5432)
				.option(ConnectionFactoryOptions.DATABASE, "hello_world")
				.option(ConnectionFactoryOptions.USER, "benchmarkdbuser")
				.option(ConnectionFactoryOptions.PASSWORD, "benchmarkdbpass")
				.option(PoolingConnectionFactoryProvider.MAX_SIZE, connectionPoolSize).build();
		ConnectionFactory connectionFactory = ConnectionFactories.get(factoryOptions);

		// Create raw HTTP servicing
		RawHttpServicerFactory serviceFactory = new RawHttpServicerFactory(serverLocation, connectionFactory,
				connectionsPerSocket);
		socketManager.bindServerSocket(serverLocation.getClusterHttpPort(), null, null, serviceFactory, serviceFactory);

		// Setup Date
		ScheduledExecutorService dateTimer = Executors.newScheduledThreadPool(1);
		dateTimer.scheduleAtFixedRate(serviceFactory.updateDate, 0, 1, TimeUnit.SECONDS);

		// Start servicing
		Runnable[] runnables = socketManager.getRunnables();
		for (int i = 0; i < runnables.length; i++) {
			executionStrategy[i].newThread(runnables[i]).start();
		}
		Thread.sleep(1000); // allow threads to start up

		// Indicate running
		System.out.println("OfficeFloor raw running on port " + serverLocation.getClusterHttpPort());
	}

	/**
	 * Raw {@link AbstractHttpServicerFactory}.
	 */
	private static class RawHttpServicerFactory extends AbstractHttpServicerFactory {

		private static HttpHeaderName NAME_SERVER = new HttpHeaderName("Server");

		private static HttpHeaderValue VALUE_SERVER = new HttpHeaderValue("O");

		private static HttpHeaderName NAME_DATE = new HttpHeaderName("Date");

		private static byte[] HELLO_WORLD = "Hello, World!".getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static final HttpHeaderValue APPLICATION_JSON = new HttpHeaderValue("application/json");

		private static final HttpHeaderValue TEXT_PLAIN = new HttpHeaderValue("text/plain");

		private static final HttpHeaderValue TEXT_HTML = new HttpHeaderValue("text/html;charset=utf-8");

		private static final String QUERIES_PATH_PREFIX = "/queries?queries=";

		private static final String UPDATE_PATH_PREFIX = "/update?queries=";

		private static final byte[] TEMPLATE_START = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
				.getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static final byte[] FORTUNE_START = "<tr><td>".getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static final byte[] FORTUNE_MIDDLE = "</td><td>".getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static final byte[] FORTUNE_END = "</td></tr>".getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static final byte[] TEMPLATE_END = "</table></body></html>"
				.getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static final R2dbcTransientResourceException THROTTLED = new R2dbcTransientResourceException();

		/**
		 * <code>Date</code> {@link HttpHeaderValue}.
		 */
		private volatile HttpHeaderValue dateHttpHeader;

		private final Runnable updateDate = () -> {
			String now = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now(ZoneOffset.UTC));
			RawHttpServicerFactory.this.dateHttpHeader = new HttpHeaderValue(now);
		};

		/**
		 * {@link ObjectMapper}.
		 */
		private final ObjectMapper objectMapper = new ObjectMapper();

		/**
		 * {@link ManagedObjectContext}.
		 */
		private static ManagedObjectContext managedObjectContext = new ManagedObjectContext() {

			@Override
			public String getBoundName() {
				return R2dbcOfficeFloorMain.class.getSimpleName();
			}

			@Override
			public Logger getLogger() {
				return logger;
			}

			@Override
			public <R, T extends Throwable> R run(ProcessSafeOperation<R, T> operation) throws T {
				return operation.run();
			}
		};

		/**
		 * {@link ConnectionFactory}.
		 */
		private final ConnectionFactory connectionFactory;

		/**
		 * {@link ThreadLocal} {@link Connection} instances.
		 */
		private final ThreadLocal<Connection[]> threadLocalConnections;

		/**
		 * {@link ThreadLocal} {@link RateLimit}.
		 */
		private final ThreadLocal<RateLimit> threadLocalRateLimit = new ThreadLocal<RateLimit>();

		/**
		 * Instantiate.
		 *
		 * @param serverLocation       {@link HttpServerLocation}.
		 * @param connectionFactory    {@link ConnectionFactory}.
		 * @param connectionsPerSocket Number of DB connections per socket.
		 */
		public RawHttpServicerFactory(HttpServerLocation serverLocation, ConnectionFactory connectionFactory,
				int connectionsPerSocket) {
			super(serverLocation, false, new HttpRequestParserMetaData(100, 1000, 1000000), null, null, true);
			this.objectMapper.registerModule(new AfterburnerModule());
			this.connectionFactory = connectionFactory;

			// Create thread local connection
			this.threadLocalConnections = new ThreadLocal<Connection[]>() {
				@Override
				protected Connection[] initialValue() {
					Connection[] connections = new Connection[connectionsPerSocket];
					for (int i = 0; i < connections.length; i++) {
						connections[i] = Mono.from(RawHttpServicerFactory.this.connectionFactory.create()).block();
					}
					return connections;
				}
			};
		}

		/**
		 * Sends the {@link HttpResponse}.
		 * 
		 * @param connection {@link ServerHttpConnection}.
		 * @throws IOException If fails to send.
		 */
		protected void send(ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection) throws IOException {
			try {
				connection.getServiceFlowCallback().run(null);
			} catch (IOException ex) {
				throw ex;
			} catch (Throwable ex) {
				throw new IOException(ex);
			}
		}

		/*
		 * =============== SocketServicerFactory =================
		 */

		@Override
		public SocketServicer<HttpRequestParser> createSocketServicer(
				RequestHandler<HttpRequestParser> requestHandler) {

			// Ensure rate limits for socket servicing thread
			// Note: will always create before servicing any requests
			if (this.threadLocalRateLimit.get() == null) {
				Connection[] connections = this.threadLocalConnections.get();
				RateLimit rateLimit = new RateLimit(requestHandler, connections);
				this.threadLocalRateLimit.set(rateLimit);
			}

			// Continue on to create socket servicer
			return super.createSocketServicer(requestHandler);
		}

		/*
		 * ===================== HttpServicer ====================
		 */

		@Override
		protected ProcessManager service(ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection)
				throws IOException {

			// Configure context
			connection.setManagedObjectContext(managedObjectContext);

			// Service the connection
			HttpRequest request = connection.getRequest();
			HttpResponse response = connection.getResponse();

			// Provider Server and Date
			HttpResponseHeaders headers = response.getHeaders();
			headers.addHeader(NAME_SERVER, VALUE_SERVER);
			headers.addHeader(NAME_DATE, this.dateHttpHeader);

			// Determine request
			String requestUri = request.getUri();
			switch (requestUri) {

			case "/plaintext":
				this.plaintext(response, connection);
				break;

			case "/json":
				this.json(response, connection);
				break;

			case "/db":
				this.db(response, connection);
				break;

			case "/fortunes":
				this.fortunes(response, connection);
				break;

			default:
				// Provide redirect
				if (requestUri.startsWith(QUERIES_PATH_PREFIX)) {
					this.queries(requestUri, response, connection);

				} else if (requestUri.startsWith(UPDATE_PATH_PREFIX)) {
					this.update(requestUri, response, connection);

				} else {
					// Unknown request
					response.setStatus(HttpStatus.NOT_FOUND);
					this.send(connection);
				}
				break;
			}

			// No process management
			return null;
		}

		private void plaintext(HttpResponse response,
				ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection) throws IOException {
			response.setContentType(TEXT_PLAIN, null);
			response.getEntity().write(HELLO_WORLD);
			this.send(connection);
		}

		private void json(HttpResponse response, ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection)
				throws IOException {
			response.setContentType(APPLICATION_JSON, null);
			this.objectMapper.writeValue(response.getEntityWriter(), new Message("Hello, World!"));
			this.send(connection);
		}

		private void db(HttpResponse response, ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection) {

			// Determine if will overload queries
			RateLimitedConnection conn = this.threadLocalRateLimit.get().getAvailableConnection(1);
			if (conn == null) {
				this.sendError(connection, THROTTLED);
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
						try {
							response.setContentType(APPLICATION_JSON, null);
							this.objectMapper.writeValue(response.getEntityWriter(), world);
							this.send(connection);
						} catch (CancelledKeyException | ClosedChannelException ex) {
							// Ignore as disconnecting client
						} catch (IOException ex) {
							ex.printStackTrace();
						}
					}, error -> {
						this.sendError(connection, error);
					}, () -> {
						conn.processed(1);
					});
		}

		private void queries(String requestUri, HttpResponse response,
				ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection) {

			// Obtain the number of queries
			String queriesCountText = requestUri.substring(QUERIES_PATH_PREFIX.length());
			int queryCount = getQueryCount(queriesCountText);

			// Determine if will overload queries
			RateLimitedConnection conn = this.threadLocalRateLimit.get().getAvailableConnection(queryCount);
			if (conn == null) {
				this.sendError(connection, THROTTLED);
				return; // rate limited
			}

			// Service
			Flux.range(1, queryCount)
					.flatMap(
							index -> conn.connection.createStatement("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
									.bind(0, ThreadLocalRandom.current().nextInt(1, 10001)).execute())
					.flatMap(result -> Flux.from(result.map((row, metadata) -> {
						Integer id = row.get(0, Integer.class);
						Integer number = row.get(1, Integer.class);
						return new World(id, number);
					}))).collectList().publishOn(conn.writeScheduler).subscribe(worlds -> {
						try {
							response.setContentType(APPLICATION_JSON, null);
							this.objectMapper.writeValue(response.getEntityWriter(), worlds);
							this.send(connection);
						} catch (CancelledKeyException | ClosedChannelException ex) {
							// Ignore as disconnecting client
						} catch (IOException ex) {
							ex.printStackTrace();
						}
					}, error -> {
						this.sendError(connection, error);
					}, () -> {
						conn.processed(queryCount);
					});
		}

		private void fortunes(HttpResponse response,
				ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection) {

			// Determine if will overload queries
			RateLimitedConnection conn = this.threadLocalRateLimit.get().getAvailableConnection(1);
			if (conn == null) {
				this.sendError(connection, THROTTLED);
				return; // rate limited
			}

			// Service
			Flux.from(conn.connection.createStatement("SELECT ID, MESSAGE FROM FORTUNE").execute())
					.flatMap(result -> Flux.from(result.map((row, metadata) -> {
						Integer id = row.get(0, Integer.class);
						String message = row.get(1, String.class);
						return new Fortune(id, message);
					}))).collectList().publishOn(conn.writeScheduler).subscribe(fortunes -> {
						try {
							// Additional fortunes
							fortunes.add(new Fortune(0, "Additional fortune added at request time."));
							Collections.sort(fortunes, (a, b) -> a.message.compareTo(b.message));

							// Send response
							response.setContentType(TEXT_HTML, null);
							ServerWriter writer = response.getEntityWriter();
							writer.write(TEMPLATE_START);
							for (Fortune fortune : fortunes) {
								writer.write(FORTUNE_START);
								int id = fortune.getId();
								writer.write(Integer.valueOf(id).toString());
								writer.write(FORTUNE_MIDDLE);
								StringEscapeUtils.ESCAPE_HTML4.translate(fortune.getMessage(), writer);
								writer.write(FORTUNE_END);
							}
							writer.write(TEMPLATE_END);
							this.send(connection);
						} catch (CancelledKeyException | ClosedChannelException ex) {
							// Ignore as disconnecting client
						} catch (IOException ex) {
							ex.printStackTrace();
						}
					}, error -> {
						this.sendError(connection, error);
					}, () -> {
						conn.processed(1);
					});
		}

		private void update(String requestUri, HttpResponse response,
				ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection) {

			// Obtain the number of queries
			String queriesCountText = requestUri.substring(UPDATE_PATH_PREFIX.length());
			int queryCount = getQueryCount(queriesCountText);
			int executeQueryCount = queryCount + 1; // select all and update

			// Determine if will overload queries
			RateLimitedConnection conn = this.threadLocalRateLimit.get().getAvailableConnection(executeQueryCount);
			if (conn == null) {
				this.sendError(connection, THROTTLED);
				return; // rate limited
			}

			// Service
			Flux.range(1, queryCount)
					.flatMap(
							index -> conn.connection.createStatement("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
									.bind(0, ThreadLocalRandom.current().nextInt(1, 10001)).execute())
					.flatMap(result -> Flux.from(result.map((row, metadata) -> {
						Integer id = row.get(0, Integer.class);
						Integer number = row.get(1, Integer.class);
						return new World(id, number);
					}))).collectList().flatMap(worlds -> {
						Collections.sort(worlds, (a, b) -> a.id - b.id);
						Batch batch = conn.connection.createBatch();
						for (World world : worlds) {
							world.randomNumber = ThreadLocalRandom.current().nextInt(1, 10001);
							batch.add("UPDATE WORLD SET RANDOMNUMBER = " + world.randomNumber + " WHERE ID = "
									+ world.id);
						}
						return Mono.from(batch.execute()).map((result) -> worlds);
					}).publishOn(conn.writeScheduler).subscribe(worlds -> {
						try {
							response.setContentType(APPLICATION_JSON, null);
							this.objectMapper.writeValue(response.getEntityWriter(), worlds);
							this.send(connection);
						} catch (CancelledKeyException | ClosedChannelException ex) {
							// Ignore as disconnecting client
						} catch (IOException ex) {
							ex.printStackTrace();
						}
					}, error -> {
						this.sendError(connection, error);
					}, () -> {
						conn.processed(executeQueryCount);
					});
		}

		private void sendError(ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection,
				Throwable failure) {
			try {

				// Setup to send response
				HttpResponse response = connection.getResponse();
				response.reset();

				// Determine type of error
				if (failure instanceof R2dbcTransientResourceException) {

					// Indicate overloaded
					response.setStatus(HttpStatus.SERVICE_UNAVAILABLE);

				} else {
					// Provide details of failure
					response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR);
					response.setContentType(TEXT_PLAIN, null);
					failure.printStackTrace(new PrintWriter(response.getEntityWriter()));
				}

				// Send error response
				this.send(connection);

			} catch (CancelledKeyException | ClosedChannelException ex) {
				// Ignore as disconnecting client
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}

		private static int getQueryCount(String queries) {
			try {
				int count = Integer.parseInt(queries);
				return (count < 1) ? 1 : (count > 500) ? 500 : count;
			} catch (NumberFormatException ex) {
				return 1;
			}
		}
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

	@Data
	public static class Message {
		private final String message;
	}

	@Data
	@AllArgsConstructor
	public static class World {

		private final int id;

		private int randomNumber;
	}

	@Data
	public static class Fortune {

		private final int id;

		private final String message;
	}
}