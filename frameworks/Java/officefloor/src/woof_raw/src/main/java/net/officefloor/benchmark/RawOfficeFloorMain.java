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
import java.nio.ByteBuffer;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collector;

import org.apache.commons.text.StringEscapeUtils;
import org.postgresql.sql2.PGConnectionProperties;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;

import jdk.incubator.sql2.AdbaType;
import jdk.incubator.sql2.Connection;
import jdk.incubator.sql2.DataSource;
import jdk.incubator.sql2.DataSourceFactory;
import jdk.incubator.sql2.Result;
import lombok.Data;
import net.officefloor.frame.api.manage.OfficeFloor;
import net.officefloor.frame.api.managedobject.ProcessAwareContext;
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
import net.officefloor.server.stream.ServerWriter;
import net.officefloor.server.stream.StreamBufferPool;
import net.officefloor.server.stream.impl.ThreadLocalStreamBufferPool;

/**
 * <p>
 * {@link SocketManager} raw performance.
 * <p>
 * Allows determining the overhead of the {@link OfficeFloor} framework.
 */
public class RawOfficeFloorMain {

	/**
	 * {@link SocketManager}.
	 */
	public static SocketManager socketManager = null;

	/**
	 * {@link DataSource}.
	 */
	public static DataSource dataSource = null;

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

		// Create the server location
		HttpServerLocation serverLocation = new HttpServerLocationImpl("localhost", port, -1);

		// Create the socket manager
		socketManager = HttpServerSocketManagedObjectSource.createSocketManager();

		// Create raw HTTP servicing
		StreamBufferPool<ByteBuffer> serviceBufferPool = new ThreadLocalStreamBufferPool(
				() -> ByteBuffer.allocateDirect(8046), Integer.MAX_VALUE, Integer.MAX_VALUE);
		RawHttpServicerFactory serviceFactory = new RawHttpServicerFactory(serverLocation, serviceBufferPool);
		socketManager.bindServerSocket(serverLocation.getClusterHttpPort(), null, null, serviceFactory, serviceFactory);

		// Create the data source
		dataSource = DataSourceFactory.newFactory("org.postgresql.sql2.PGDataSourceFactory").builder()
				.url("jdbc:postgresql://" + getProperty("db-server", "tfb-database") + ":"
						+ getProperty("db-port", "5432") + "/" + getProperty("db-name", "hello_world"))
				.connectionProperty(PGConnectionProperties.USER, getProperty("db-username", "benchmarkdbuser"))
				.connectionProperty(PGConnectionProperties.PASSWORD, getProperty("db-password", "benchmarkdbpass"))
				.build();

		// Setup Date
		Timer dateTimer = new Timer(true);
		dateTimer.schedule(serviceFactory.updateDate, 0, 1000);

		// Start servicing
		Executor executor = Executors.newCachedThreadPool();
		for (Runnable runnable : socketManager.getRunnables()) {
			executor.execute(runnable);
		}

		// Indicate running
		System.out.println("OfficeFloor raw running");
	}

	/**
	 * Obtains the property.
	 * 
	 * @param name         Name of the property.
	 * @param defaultValue Default value for the property.
	 * @return Property value.
	 */
	private static String getProperty(String name, String defaultValue) {
		String value = System.getenv(name);
		return value != null ? value : defaultValue;
	}

	/**
	 * Raw {@link AbstractHttpServicerFactory}.
	 */
	private static class RawHttpServicerFactory extends AbstractHttpServicerFactory {

		private static HttpHeaderName NAME_SERVER = new HttpHeaderName("Server");

		private static HttpHeaderValue VALUE_SERVER = new HttpHeaderValue("OF");

		private static HttpHeaderName NAME_DATE = new HttpHeaderName("Date");

		private static byte[] HELLO_WORLD = "Hello, World!".getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static HttpHeaderValue APPLICATION_JSON = new HttpHeaderValue("application/json");

		private static final HttpHeaderValue TEXT_PLAIN = new HttpHeaderValue("text/plain");

		private static final HttpHeaderValue TEXT_HTML = new HttpHeaderValue("text/html;charset=utf-8");

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

		private static final ThreadLocal<Connection> threadLocalConnection = new ThreadLocal<>() {
			@Override
			protected Connection initialValue() {
				return RawOfficeFloorMain.dataSource.getConnection();
			}
		};

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
		 * {@link ProcessAwareContext}.
		 */
		private static ProcessAwareContext processAwareContext = new ProcessAwareContext() {
			@Override
			public <R, T extends Throwable> R run(ProcessSafeOperation<R, T> operation) throws T {
				return operation.run();
			}
		};

		/**
		 * Instantiate.
		 *
		 * @param serverLocation    {@link HttpServerLocation}.
		 * @param serviceBufferPool {@link StreamBufferPool}.
		 */
		public RawHttpServicerFactory(HttpServerLocation serverLocation,
				StreamBufferPool<ByteBuffer> serviceBufferPool) {
			super(serverLocation, false, new HttpRequestParserMetaData(100, 1000, 1000000), serviceBufferPool, 1000,
					null, null, true);
			this.objectMapper.registerModule(new AfterburnerModule());
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
		protected void service(ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection)
				throws IOException {

			// Configure process awareness
			connection.setProcessAwareContext(processAwareContext);

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
				threadLocalConnection.get().<World>rowOperation("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
						.set("$1", ThreadLocalRandom.current().nextInt(1, 10000)).collect(dbCollector()).submit()
						.getCompletionStage().thenAcceptAsync((world) -> {
							try {
								response.setContentType(APPLICATION_JSON, null);
								this.objectMapper.writeValue(response.getEntityWriter(), world);
								this.send(connection);
							} catch (IOException ex) {
								// TODO handle error
								ex.printStackTrace();
							}
						});
				break;

			case "/fortunes":
				threadLocalConnection.get().<List<Fortune>>rowOperation("SELECT ID, MESSAGE FROM FORTUNE")
						.collect(fortunesCollector()).submit().getCompletionStage().thenAcceptAsync((fortunes) -> {
							try {
								fortunes.add(new Fortune(0, "Additional fortune added at request time."));
								response.setContentType(TEXT_HTML, null);
								ServerWriter writer = response.getEntityWriter();
								writer.write(TEMPLATE_START);
								Collections.sort(fortunes);
								for (int i = 0; i < fortunes.size(); i++) {
									Fortune fortune = fortunes.get(i);
									writer.write(FORTUNE_START);
									int id = fortune.getId();
									writer.write(Integer.valueOf(id).toString());
									writer.write(FORTUNE_MIDDLE);
									StringEscapeUtils.ESCAPE_HTML4.translate(fortune.getMessage(), writer);
									writer.write(FORTUNE_END);
								}
								writer.write(TEMPLATE_END);
								this.send(connection);
							} catch (IOException ex) {
								// TODO handle error
								ex.printStackTrace();
							}
						});
				break;

			default:
				// Handle query parameters
				int queryStart = requestUri.indexOf('?');
				String path = requestUri.substring(0, queryStart);
				int queryCount = getQueryCount(requestUri, queryStart);
				Connection db = threadLocalConnection.get();
				List<World> worlds = new ArrayList<>();
				ThreadLocalRandom random = ThreadLocalRandom.current();
				switch (path) {

				case "/queries":
					for (int i = 0; i < queryCount; i++) {
						db.<World>rowOperation("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
								.set("$1", random.nextInt(1, 10000)).collect(dbCollector()).submit()
								.getCompletionStage().thenAcceptAsync((world) -> {
									try {
										worlds.add(world);
										if (worlds.size() >= queryCount) {
											response.setContentType(APPLICATION_JSON, null);
											this.objectMapper.writeValue(response.getEntityWriter(), worlds);
											this.send(connection);
										}
									} catch (IOException ex) {
										// TODO handle error
										ex.printStackTrace();
									}
								});
					}
					break;

				case "/update":
					for (int i = 0; i < queryCount; i++) {
						db.<World>rowOperation("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
								.set("$1", random.nextInt(1, 10000)).collect(dbCollector()).submit()
								.getCompletionStage().thenAcceptAsync((world) -> {
									try {
										world = new World(world.id, random.nextInt(1, 10000));
										worlds.add(world);
										if (worlds.size() >= queryCount) {
											response.setContentType(APPLICATION_JSON, null);
											this.objectMapper.writeValue(response.getEntityWriter(), worlds);
											for (int u = 0; u < worlds.size(); u++) {
												World update = worlds.get(u);
												db.rowCountOperation("UPDATE WORLD SET RANDOMNUMBER = $1 WHERE ID = $2")
														.set("$1", update.randomNumber, AdbaType.INTEGER)
														.set("$2", update.id, AdbaType.INTEGER).submit();
											}
											this.send(connection);
										}
									} catch (IOException ex) {
										// TODO handle error
										ex.printStackTrace();
									}
								});
					}
					break;

				default:
					// Unknown request
					response.setStatus(HttpStatus.NOT_FOUND);
					this.send(connection);
					break;
				}
			}
		}
	}

	@Data
	public static class Message {
		private final String message;
	}

	@Data
	public static class Fortune implements Comparable<Fortune> {
		private final int id;
		private final String message;

		@Override
		public int compareTo(Fortune o) {
			return this.getMessage().compareTo(o.getMessage());
		}
	}

	@Data
	public static class World {
		private final int id;
		private final int randomNumber;
	}

	public static Collector<Result.RowColumn, World[], World> dbCollector() {
		return Collector.of(() -> new World[1],
				(a, r) -> a[0] = new World(r.at(1).get(int.class), r.at(2).get(int.class)), (l, r) -> null, a -> a[0]);
	}

	public static Collector<Result.RowColumn, List<Fortune>, List<Fortune>> fortunesCollector() {
		return Collector.of(() -> new ArrayList<>(),
				(a, r) -> a.add(new Fortune(r.at(1).get(int.class), r.at(2).get(String.class))), (l, r) -> null,
				a -> a);
	}

	public static int getQueryCount(String requestPath, int queryIndex) {
		if (queryIndex < 0) {
			return 1;
		}
		int countIndex = queryIndex + "queries=".length();
		try {
			// +1 to not include '='
			String countValue = requestPath.substring(countIndex + 1);
			int count = Integer.parseInt(countValue);
			return (count < 1) ? 1 : (count > 500) ? 500 : count;
		} catch (Exception ex) {
			return 1;
		}
	}

}
