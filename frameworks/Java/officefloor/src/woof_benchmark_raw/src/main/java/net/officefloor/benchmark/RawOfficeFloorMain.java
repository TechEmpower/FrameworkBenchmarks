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
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import org.apache.commons.text.StringEscapeUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;
import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;

import io.r2dbc.spi.Batch;
import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.ConnectionFactoryOptions;
import io.r2dbc.spi.R2dbcTransientResourceException;
import lombok.AllArgsConstructor;
import lombok.Data;
import net.officefloor.frame.api.manage.OfficeFloor;
import net.officefloor.frame.api.manage.ProcessManager;
import net.officefloor.frame.api.managedobject.ManagedObjectContext;
import net.officefloor.frame.api.managedobject.ProcessSafeOperation;
import net.officefloor.server.SocketManager;
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
import net.officefloor.server.http.parse.HttpRequestParser.HttpRequestParserMetaData;
import net.officefloor.server.stream.StreamBufferPool;
import net.officefloor.server.stream.impl.ThreadLocalStreamBufferPool;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * <p>
 * {@link SocketManager} raw performance.
 * <p>
 * Allows determining the overhead of the {@link OfficeFloor} framework.
 */
public class RawOfficeFloorMain {

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
	private static Logger logger = Logger.getLogger(RawOfficeFloorMain.class.getName());

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

		// Build the connection pool
		ConnectionFactoryOptions factoryOptions = ConnectionFactoryOptions.builder()
				.option(ConnectionFactoryOptions.DRIVER, "pool").option(ConnectionFactoryOptions.PROTOCOL, "postgresql")
				.option(ConnectionFactoryOptions.HOST, server).option(ConnectionFactoryOptions.PORT, 5432)
				.option(ConnectionFactoryOptions.DATABASE, "hello_world")
				.option(ConnectionFactoryOptions.USER, "benchmarkdbuser")
				.option(ConnectionFactoryOptions.PASSWORD, "benchmarkdbpass").build();
		ConnectionFactory connectionFactory = ConnectionFactories.get(factoryOptions);

		// Create the server location
		HttpServerLocation serverLocation = new HttpServerLocationImpl("localhost", port, -1);

		// Create the execution strategy
		ThreadFactory[] executionStrategy = new ThreadFactory[Runtime.getRuntime().availableProcessors()];
		for (int i = 0; i < executionStrategy.length; i++) {
			executionStrategy[i] = (runnable) -> new Thread(runnable);
		}
		System.out.println("Using " + executionStrategy.length + " executors");

		// Create the socket manager
		socketManager = HttpServerSocketManagedObjectSource.createSocketManager(executionStrategy);

		// Create raw HTTP servicing
		StreamBufferPool<ByteBuffer> serviceBufferPool = new ThreadLocalStreamBufferPool(
				() -> ByteBuffer.allocateDirect(8046), Integer.MAX_VALUE, Integer.MAX_VALUE);
		RawHttpServicerFactory serviceFactory = new RawHttpServicerFactory(serverLocation, serviceBufferPool,
				connectionFactory);
		socketManager.bindServerSocket(serverLocation.getClusterHttpPort(), null, null, serviceFactory, serviceFactory);

		// Setup Date
		Timer dateTimer = new Timer(true);
		dateTimer.schedule(serviceFactory.updateDate, 0, 1000);

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

		private static HttpHeaderValue VALUE_SERVER = new HttpHeaderValue("OF");

		private static HttpHeaderName NAME_DATE = new HttpHeaderName("Date");

		private static byte[] HELLO_WORLD = "Hello, World!".getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static final HttpHeaderValue APPLICATION_JSON = new HttpHeaderValue("application/json");

		private static final HttpHeaderValue TEXT_PLAIN = new HttpHeaderValue("text/plain");

		private static final HttpHeaderValue TEXT_HTML = new HttpHeaderValue("text/html;charset=utf-8");

		private static final String QUERIES_PATH_PREFIX = "/queries?queries=";

		private static final String UPDATE_PATH_PREFIX = "/update?queries=";

		private static final R2dbcTransientResourceException THROTTLED = new R2dbcTransientResourceException();

		private static final int LARGE_QUERY = 100;

		/**
		 * <code>Date</code> {@link HttpHeaderValue}.
		 */
		private volatile HttpHeaderValue dateHttpHeader;

		private final TimerTask updateDate = new TimerTask() {
			@Override
			public void run() {
				String now = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now(ZoneOffset.UTC));
				RawHttpServicerFactory.this.dateHttpHeader = new HttpHeaderValue(now);
			}
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
				return RawOfficeFloorMain.class.getSimpleName();
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
		 * {@link ThreadLocal} {@link Connection}.
		 */
		private final ThreadLocal<Connection> threadLocalConnection;

		/**
		 * db {@link ThreadLocalRateLimit}.
		 */
		private final ThreadLocalRateLimit dbRateLimit = new ThreadLocalRateLimit();

		/**
		 * queries {@link ThreadLocalRateLimit}.
		 */
		private final ThreadLocalRateLimit queriesRateLimit = new ThreadLocalRateLimit();

		/**
		 * fortunes {@link ThreadLocalRateLimit}.
		 */
		private final ThreadLocalRateLimit fortunesRateLimit = new ThreadLocalRateLimit();

		/**
		 * updates {@link ThreadLocalRateLimit}.
		 */
		private final ThreadLocalRateLimit updatesRateLimit = new ThreadLocalRateLimit();

		/**
		 * {@link Mono} to service /db.
		 */
		private final Mono<World> db;

		/**
		 * {@link Mustache} for /fortunes.
		 */
		private final Mustache fortuneMustache;

		/**
		 * {@link Mono} to service /fortunes.
		 */
		private final Mono<List<Fortune>> fortunes;

		/**
		 * Instantiate.
		 *
		 * @param serverLocation    {@link HttpServerLocation}.
		 * @param serviceBufferPool {@link StreamBufferPool}.
		 * @param connectionFactory {@link ConnectionFactory}.
		 */
		public RawHttpServicerFactory(HttpServerLocation serverLocation, StreamBufferPool<ByteBuffer> serviceBufferPool,
				ConnectionFactory connectionFactory) {
			super(serverLocation, false, new HttpRequestParserMetaData(100, 1000, 1000000), serviceBufferPool, null,
					null, true);
			this.objectMapper.registerModule(new AfterburnerModule());
			this.connectionFactory = connectionFactory;

			// Create thread local connection
			this.threadLocalConnection = new ThreadLocal<Connection>() {
				@Override
				protected Connection initialValue() {
					return Mono.from(RawHttpServicerFactory.this.connectionFactory.create()).block();
				}
			};

			// Create the db logic
			this.db = Mono
					.from(this.threadLocalConnection.get()
							.createStatement("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
							.bind(0, ThreadLocalRandom.current().nextInt(1, 10001)).execute())
					.flatMap(result -> Mono.from(result.map((row, metadata) -> {
						Integer id = row.get(0, Integer.class);
						Integer number = row.get(1, Integer.class);
						return new World(id, number);
					})));

			// Load the mustache fortunes template
			MustacheFactory mustacheFactory = new DefaultMustacheFactory() {
				@Override
				public void encode(String value, Writer writer) {
					try {
						StringEscapeUtils.ESCAPE_HTML4.translate(value, writer);
					} catch (IOException ex) {
						ex.printStackTrace();
					}
				}
			};
			this.fortuneMustache = mustacheFactory.compile("fortunes.mustache");

			// Create the fortunes logic
			this.fortunes = Flux
					.from(this.threadLocalConnection.get().createStatement("SELECT ID, MESSAGE FROM FORTUNE").execute())
					.flatMap(result -> Flux.from(result.map((row, metadata) -> {
						Integer id = row.get(0, Integer.class);
						String message = row.get(1, String.class);
						return new Fortune(id, message);
					}))).collectList();
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
			} catch (Throwable ex) {
				throw new IOException(ex);
			}
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
			RateLimit rateLimit = this.dbRateLimit.get();
			if (rateLimit.isLimit(1)) {
				this.sendError(connection, THROTTLED);
				return; // rate limited
			}

			// Service
			this.db.subscribe(world -> {
				try {
					response.setContentType(APPLICATION_JSON, null);
					this.objectMapper.writeValue(response.getEntityWriter(), world);
					this.send(connection);
				} catch (IOException ex) {
					ex.printStackTrace();
				}
			}, error -> {
				this.sendError(connection, error);
			}, () -> {
				rateLimit.processed(1);
			});
		}

		private void queries(String requestUri, HttpResponse response,
				ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection) {

			// Obtain the number of queries
			String queriesCountText = requestUri.substring(QUERIES_PATH_PREFIX.length());
			int queryCount = getQueryCount(queriesCountText);

			// Determine if will overload queries
			RateLimit rateLimit = this.queriesRateLimit.get();
			if (queryCount < LARGE_QUERY) {
				if (rateLimit.isLimit(queryCount)) {
					this.sendError(connection, THROTTLED);
					return; // rate limited
				}
			}

			// Service
			Connection dbConn = this.threadLocalConnection.get();
			Flux.range(1, queryCount)
					.flatMap(index -> dbConn.createStatement("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
							.bind(0, ThreadLocalRandom.current().nextInt(1, 10001)).execute())
					.flatMap(result -> Flux.from(result.map((row, metadata) -> {
						Integer id = row.get(0, Integer.class);
						Integer number = row.get(1, Integer.class);
						return new World(id, number);
					}))).collectList().subscribe(worlds -> {
						try {
							response.setContentType(APPLICATION_JSON, null);
							this.objectMapper.writeValue(response.getEntityWriter(), worlds);
							this.send(connection);
						} catch (IOException ex) {
							ex.printStackTrace();
						}
					}, error -> {
						this.sendError(connection, error);
					}, () -> {
						if (queryCount < LARGE_QUERY) {
							rateLimit.processed(queryCount);
						}
					});
		}

		private void fortunes(HttpResponse response,
				ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection) {

			// Determine if will overload queries
			RateLimit rateLimit = this.fortunesRateLimit.get();
			if (rateLimit.isLimit(1)) {
				this.sendError(connection, THROTTLED);
				return; // rate limited
			}

			// Service
			this.fortunes.subscribe(fortunes -> {
				try {
					// Additional fortunes
					fortunes.add(new Fortune(0, "Additional fortune added at request time."));
					Collections.sort(fortunes, (a, b) -> a.message.compareTo(b.message));

					// Send response
					response.setContentType(TEXT_HTML, null);
					this.fortuneMustache.execute(response.getEntityWriter(), fortunes);
					this.send(connection);
				} catch (IOException ex) {
					ex.printStackTrace();
				}
			}, error -> {
				this.sendError(connection, error);
			}, () -> {
				rateLimit.processed(1);
			});
		}

		private void update(String requestUri, HttpResponse response,
				ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection) {

			// Obtain the number of queries
			String queriesCountText = requestUri.substring(UPDATE_PATH_PREFIX.length());
			int queryCount = getQueryCount(queriesCountText);

			// Determine if will overload queries
			int executedQueryCount = queryCount * 2; // select and update
			RateLimit rateLimit = this.updatesRateLimit.get();
			if (queryCount < LARGE_QUERY) {
				if (rateLimit.isLimit(executedQueryCount)) {
					this.sendError(connection, THROTTLED);
					return; // rate limited
				}
			}

			// Service
			Connection db = this.threadLocalConnection.get();
			Flux.range(1, queryCount)
					.flatMap(index -> db.createStatement("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
							.bind(0, ThreadLocalRandom.current().nextInt(1, 10001)).execute())
					.flatMap(result -> Flux.from(result.map((row, metadata) -> {
						Integer id = row.get(0, Integer.class);
						Integer number = row.get(1, Integer.class);
						return new World(id, number);
					}))).collectList().flatMap(worlds -> {
						Collections.sort(worlds, (a, b) -> a.id - b.id);
						Batch batch = db.createBatch();
						for (World world : worlds) {
							world.randomNumber = ThreadLocalRandom.current().nextInt(1, 10001);
							batch.add("UPDATE WORLD SET RANDOMNUMBER = " + world.randomNumber + " WHERE ID = "
									+ world.id);
						}
						return Mono.from(batch.execute()).map((result) -> worlds);
					}).subscribe(worlds -> {
						try {
							response.setContentType(APPLICATION_JSON, null);
							this.objectMapper.writeValue(response.getEntityWriter(), worlds);
							this.send(connection);
						} catch (IOException ex) {
							ex.printStackTrace();
						}
					}, error -> {
						this.sendError(connection, error);
					}, () -> {
						if (queryCount < LARGE_QUERY) {
							rateLimit.processed(executedQueryCount);
						}
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

	private static class ThreadLocalRateLimit extends ThreadLocal<RateLimit> {
		@Override
		protected RateLimit initialValue() {
			return new RateLimit();
		}
	}

	private static class RateLimit {

		private final int INITIAL_REQUEST_COUNT = 738 / Runtime.getRuntime().availableProcessors();

		private int requestCount = 0;

		private final AtomicInteger activeQueries = new AtomicInteger(0);

		private boolean isActiveLimit = false;

		public boolean isLimit(int queryCount) {

			// Increment the request count
			this.requestCount = this.requestCount + ((this.requestCount > INITIAL_REQUEST_COUNT) ? 0 : 1);

			// Ensure initial requests are processed
			do {

				// Determine if query count reached
				this.activeQueries.updateAndGet((count) -> {
					int newCount = count + queryCount;
					if (newCount > QUERY_BUFFER_SIZE) {
						// Max limit reached
						this.isActiveLimit = true;
						return count;
					} else {
						// Allow input
						this.isActiveLimit = false;
						return newCount;
					}
				});
				if (this.requestCount > INITIAL_REQUEST_COUNT) {
					// Initial requests processed, so start possible throttling
					return this.isActiveLimit;
				}

				// Allow some processing time if limit hit
				if (this.isActiveLimit) {
					try {
						Thread.sleep(1);
					} catch (InterruptedException ex) {
						// continue processing
					}
				}

			} while (this.isActiveLimit);
			return false; // below limit
		}

		public void processed(int queryCount) {
			this.activeQueries.updateAndGet((count) -> count - queryCount);
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