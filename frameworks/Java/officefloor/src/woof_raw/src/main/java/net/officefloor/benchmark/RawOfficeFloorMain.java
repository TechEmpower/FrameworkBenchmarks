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
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadLocalRandom;

import org.apache.commons.text.StringEscapeUtils;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.reactiverse.pgclient.PgPoolOptions;
import io.reactiverse.reactivex.pgclient.PgClient;
import io.reactiverse.reactivex.pgclient.PgIterator;
import io.reactiverse.reactivex.pgclient.PgPool;
import io.reactiverse.reactivex.pgclient.Tuple;
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
	 * {@link PgPool}.
	 */
	public static PgPool client;

	/**
	 * Run application.
	 */
	public static void main(String[] args) throws Exception {

		// Ensure previous socket manager shutdown (typically from tests)
		if (socketManager != null) {
			socketManager.shutdown();
		}

		// Create the server location
		HttpServerLocation serverLocation = new HttpServerLocationImpl("localhost", 8080, -1);

		// Create the socket manager
		socketManager = HttpServerSocketManagedObjectSource.createSocketManager();

		// Create raw HTTP servicing
		StreamBufferPool<ByteBuffer> serviceBufferPool = new ThreadLocalStreamBufferPool(
				() -> ByteBuffer.allocateDirect(8046), Integer.MAX_VALUE, Integer.MAX_VALUE);
		RawHttpServicerFactory serviceFactory = new RawHttpServicerFactory(serverLocation, serviceBufferPool);
		socketManager.bindServerSocket(serverLocation.getClusterHttpPort(), null, null, serviceFactory, serviceFactory);

		// Setup Date
		Timer dateTimer = new Timer(true);
		dateTimer.schedule(serviceFactory.updateDate, 0, 1000);

		// Create reactive connection
		InputStream configuration = RawOfficeFloorMain.class.getResourceAsStream("/datasource.properties");
		Properties properties = new Properties();
		properties.load(configuration);
		PgPoolOptions options = new PgPoolOptions().setPort(Integer.parseInt(properties.getProperty("port")))
				.setHost(properties.getProperty("server")).setDatabase(properties.getProperty("database"))
				.setUser(properties.getProperty("user")).setPassword(properties.getProperty("password"))
				.setMaxSize(Integer.parseInt(properties.getProperty("pool.size"))).setCachePreparedStatements(true);
		client = PgClient.pool(options);

		// Start servicing
		Executor executor = Executors.newCachedThreadPool();
		for (Runnable runnable : socketManager.getRunnables()) {
			executor.execute(runnable);
		}

		// Indicate running
		System.out.println("OfficeFloor raw running");
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
		 * @param serverLocation
		 *            {@link HttpServerLocation}.
		 * @param serviceBufferPool
		 *            {@link StreamBufferPool}.
		 */
		public RawHttpServicerFactory(HttpServerLocation serverLocation,
				StreamBufferPool<ByteBuffer> serviceBufferPool) {
			super(serverLocation, false, new HttpRequestParserMetaData(100, 1000, 1000000), serviceBufferPool, 1000,
					null, null, true);
		}

		/*
		 * ===================== HttpServicer ====================
		 */

		@Override
		protected void service(ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection)
				throws IOException {
			ThreadLocalRandom random = ThreadLocalRandom.current();

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
				sendResponse(connection);
				break;

			case "/json":
				response.setContentType(APPLICATION_JSON, null);
				this.objectMapper.writeValue(response.getEntityWriter(), new Message("Hello, World!"));
				sendResponse(connection);
				break;

			case "/db":
				client.preparedQuery("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1",
						Tuple.of(random.nextInt(1, 10000)), (result) -> {
							try {
								if (result.failed()) {
									sendError(HttpStatus.INTERNAL_SERVER_ERROR, response, connection);
									return;
								}
								PgIterator resultSet = result.result().iterator();
								Tuple row = resultSet.next();
								World world = new World(row.getInteger(0), row.getInteger(1));
								response.setContentType(APPLICATION_JSON, null);
								objectMapper.writeValue(response.getEntity(), world);
								sendResponse(connection);
							} catch (IOException ex) {
							}
						});
				break;

			case "/fortunes":
				client.preparedQuery("SELECT ID, MESSAGE FROM FORTUNE", (result) -> {
					try {
						if (result.failed()) {
							sendError(HttpStatus.INTERNAL_SERVER_ERROR, response, connection);
							return;
						}
						List<Fortune> fortunes = new ArrayList<>();
						PgIterator resultSet = result.result().iterator();
						while (resultSet.hasNext()) {
							Tuple row = resultSet.next();
							fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
						}
						fortunes.add(new Fortune(0, "Additional fortune added at request time."));
						Collections.sort(fortunes);
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
						writer.flush();
					} catch (IOException ex) {
					}
				});
				break;

			default:
				if (requestUri.startsWith("/queries")) {
					int queries = getQueries(requestUri);
					int[] count = new int[] { 0 };
					World[] worlds = new World[queries];
					for (int i = 0; i < queries; i++) {
						final int index = i;
						client.preparedQuery("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1",
								Tuple.of(random.nextInt(1, 10000)), (result) -> {
									try {
										if (count[0] != -1) {
											if (result.failed()) {
												count[0] = -1;
												sendError(HttpStatus.INTERNAL_SERVER_ERROR, response, connection);
												return;
											}
											Tuple row = result.result().iterator().next();
											worlds[index] = new World(row.getInteger(0), row.getInteger(1));
											if (count[0] == queries) {
												response.setContentType(APPLICATION_JSON, null);
												objectMapper.writeValue(response.getEntity(), worlds);
												sendResponse(connection);
											}
										}
									} catch (IOException ex) {
									}
								});
					}

				} else if (requestUri.startsWith("/updates")) {
					int queries = getQueries(requestUri);
					int[] count = new int[] { 0 };
					World[] worlds = new World[queries];
					for (int i = 0; i < queries; i++) {
						final int index = i;
						client.preparedQuery("SELECT ID FROM WORLD WHERE ID = $1", Tuple.of(random.nextInt(1, 10000)),
								(result) -> {
									try {
										if (count[0] != -1) {
											if (result.failed()) {
												count[0] = -1;
												sendError(HttpStatus.INTERNAL_SERVER_ERROR, response, connection);
												return;
											}
											Tuple row = result.result().iterator().next();
											worlds[index] = new World(row.getInteger(0), random.nextInt(1, 10000));
											if (count[0] == queries) {
												Arrays.sort(worlds);
												List<Tuple> updates = new ArrayList<>(queries);
												for (int u = 0; u < queries; u++) {
													updates.add(Tuple.of(worlds[u].getId(), random.nextInt(1, 10000)));
												}
												client.preparedBatch("UPDATE WORLD SET RANDOMNUMBER = $1 WHERE ID = $2",
														updates, (updateResult) -> {
															try {
																if (updateResult.failed()) {
																	sendError(HttpStatus.INTERNAL_SERVER_ERROR,
																			response, connection);
																	return;
																}
																response.setContentType(APPLICATION_JSON, null);
																objectMapper.writeValue(response.getEntity(), worlds);
																sendResponse(connection);
															} catch (IOException ex) {
															}
														});
											}
										}
									} catch (IOException ex) {
									}
								});
					}

				} else {
					response.setStatus(HttpStatus.NOT_FOUND);
					sendResponse(connection);
				}
				break;
			}
		}

		private static int getQueries(String requestUri) {
			int separatorIndex = requestUri.lastIndexOf('=');
			String value = "1";
			if (separatorIndex >= 0) {
				value = requestUri.substring(separatorIndex + 1);
			}
			int count;
			try {
				count = Integer.parseInt(value);
			} catch (NumberFormatException ex) {
				return 1;
			}
			return count < 1 ? 1 : (count > 500 ? 500 : count);
		}

		private static void sendResponse(ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection)
				throws IOException {
			try {
				connection.getServiceFlowCallback().run(null);
			} catch (Throwable ex) {
				throw new IOException(ex);
			}
		}

		private static void sendError(HttpStatus errorStatus, HttpResponse response,
				ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection) throws IOException {
			response.setStatus(errorStatus);
			sendResponse(connection);
		}
	}

	@Data
	public static class Message {
		private final String message;
	}

}