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
import java.nio.ByteBuffer;
import java.nio.channels.CancelledKeyException;
import java.nio.channels.ClosedChannelException;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.apache.commons.text.StringEscapeUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;

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

/**
 * <p>
 * {@link SocketManager} raw performance.
 * <p>
 * Allows determining the overhead of the {@link OfficeFloor} framework.
 */
public abstract class RawWoof {

	/**
	 * {@link SocketManager}.
	 */
	public static SocketManager socketManager = null;

	/**
	 * {@link Logger}.
	 */
	private static Logger logger = Logger.getLogger(RawWoof.class.getName());

	/**
	 * Run application.
	 * 
	 * @param args              Command line arguments.
	 * @param operationsFactory {@link DatabaseOperationsFactory}.
	 */
	public static void run(String[] args, DatabaseOperationsFactory operationsFactory) throws Exception {

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

		// Create the server location
		HttpServerLocation serverLocation = new HttpServerLocationImpl("localhost", port, -1);

		// Create a thread factory per logical CPU
		ThreadCompletionListener[] threadCompletionListenerCapture = new ThreadCompletionListener[] { null };
		ThreadFactory[] executionStrategy = RawWoofThreadAffinity
				.createThreadFactories(() -> (ThreadLocalStreamBufferPool) threadCompletionListenerCapture[0]);
		System.out.println("Using " + executionStrategy.length + " executors");

		// Create the socket manager
		socketManager = HttpServerSocketManagedObjectSource.createSocketManager(executionStrategy,
				(threadCompletionListener) -> threadCompletionListenerCapture[0] = threadCompletionListener);

		// Create the database operations
		DatabaseOperations operations = operationsFactory.createDatabaseOperations(executionStrategy.length, server,
				5432, "hello_world", "benchmarkdbuser", "benchmarkdbpass");

		// Create raw HTTP servicing
		RawHttpServicerFactory serviceFactory = new RawHttpServicerFactory(serverLocation, operations);
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
		System.out.println("OfficeFloor running on port " + serverLocation.getClusterHttpPort());
	}

	/**
	 * Raw {@link AbstractHttpServicerFactory}.
	 */
	private static class RawHttpServicerFactory extends AbstractHttpServicerFactory
			implements DatabaseOperationsContext {

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

		private static final byte[] FORTUNE_START = "<tr><td>"
				.getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static final byte[] FORTUNE_MIDDLE = "</td><td>"
				.getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static final byte[] FORTUNE_END = "</td></tr>"
				.getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static final byte[] TEMPLATE_END = "</table></body></html>"
				.getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static final TransientResourceException THROTTLED = new TransientResourceException();

		private static class TransientResourceException extends Exception {
			private static final long serialVersionUID = 1L;
		}

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
				return RawWoof.class.getSimpleName();
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

		private static int getQueryCount(String queries) {
			try {
				int count = Integer.parseInt(queries);
				return (count < 1) ? 1 : (count > 500) ? 500 : count;
			} catch (NumberFormatException ex) {
				return 1;
			}
		}

		/**
		 * {@link DatabaseOperations}.
		 */
		private final DatabaseOperations databaseOperations;

		/**
		 * Instantiate.
		 *
		 * @param serverLocation {@link HttpServerLocation}.
		 * @param operations     {@link DatabaseOperations}.
		 */
		public RawHttpServicerFactory(HttpServerLocation serverLocation, DatabaseOperations operations) {
			super(serverLocation, false, new HttpRequestParserMetaData(100, 1000, 1000000), null, null, true);
			this.objectMapper.registerModule(new AfterburnerModule());
			this.databaseOperations = operations;
		}

		/**
		 * Sends the {@link HttpResponse}.
		 * 
		 * @param connection {@link ServerHttpConnection}.
		 * @throws IOException If fails to send.
		 */
		protected void send(ServerHttpConnection connection) throws IOException {
			try {
				@SuppressWarnings("unchecked")
				ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> rawConnection = (ProcessAwareServerHttpConnectionManagedObject<ByteBuffer>) connection;
				rawConnection.getServiceFlowCallback().run(null);
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

			// Set up the thread
			this.databaseOperations.threadSetup(requestHandler);

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
				response.setContentType(TEXT_PLAIN, null);
				response.getEntity().write(HELLO_WORLD);
				this.send(connection);
				break;

			case "/json":
				response.setContentType(APPLICATION_JSON, null);
				this.objectMapper.writeValue(response.getEntityWriter(), new Message("Hello, World!"));
				this.send(connection);
				break;

			case "/db":
				this.databaseOperations.db(response, connection, this);
				break;

			case "/fortunes":
				this.databaseOperations.fortunes(response, connection, this);
				break;

			default:
				// Provide redirect
				if (requestUri.startsWith(QUERIES_PATH_PREFIX)) {
					// Obtain the number of queries
					String queriesCountText = requestUri.substring(QUERIES_PATH_PREFIX.length());
					int queryCount = getQueryCount(queriesCountText);

					// Undertake queries
					this.databaseOperations.queries(queryCount, response, connection, this);

				} else if (requestUri.startsWith(UPDATE_PATH_PREFIX)) {
					// Obtain the number of queries
					String queriesCountText = requestUri.substring(UPDATE_PATH_PREFIX.length());
					int queryCount = getQueryCount(queriesCountText);

					// Undertake update
					this.databaseOperations.update(queryCount, response, connection, this);

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

		/*
		 * ==================== DatabaseOperationsContext =====================
		 */

		@Override
		public Exception getTransientResourceException() {
			return THROTTLED;
		}

		@Override
		public void dbSend(HttpResponse response, ServerHttpConnection connection, World world) {
			try {
				response.setContentType(APPLICATION_JSON, null);
				this.objectMapper.writeValue(response.getEntityWriter(), world);
				this.send(connection);
			} catch (CancelledKeyException | ClosedChannelException ex) {
				// Ignore as disconnecting client
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}

		@Override
		public void queriesSend(HttpResponse response, ServerHttpConnection connection, List<World> worlds) {
			try {
				response.setContentType(APPLICATION_JSON, null);
				this.objectMapper.writeValue(response.getEntityWriter(), worlds);
				this.send(connection);
			} catch (CancelledKeyException | ClosedChannelException ex) {
				// Ignore as disconnecting client
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}

		@Override
		public void fortunesSend(HttpResponse response, ServerHttpConnection connection, List<Fortune> fortunes) {
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
					int id = fortune.id;
					writer.write(Integer.valueOf(id).toString());
					writer.write(FORTUNE_MIDDLE);
					StringEscapeUtils.ESCAPE_HTML4.translate(fortune.message, writer);
					writer.write(FORTUNE_END);
				}
				writer.write(TEMPLATE_END);
				this.send(connection);
			} catch (CancelledKeyException | ClosedChannelException ex) {
				// Ignore as disconnecting client
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}

		@Override
		public void updateSend(HttpResponse response, ServerHttpConnection connection, List<World> worlds) {
			try {
				response.setContentType(APPLICATION_JSON, null);
				this.objectMapper.writeValue(response.getEntityWriter(), worlds);
				this.send(connection);
			} catch (CancelledKeyException | ClosedChannelException ex) {
				// Ignore as disconnecting client
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}

		@Override
		public void sendError(ServerHttpConnection connection, Throwable failure) {
			try {

				// Setup to send response
				HttpResponse response = connection.getResponse();
				response.reset();

				// Determine type of error
				if (failure instanceof TransientResourceException) {

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
	}

}