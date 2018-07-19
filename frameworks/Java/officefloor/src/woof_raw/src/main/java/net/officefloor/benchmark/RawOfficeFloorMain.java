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
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;

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
	 * Raw {@link AbstractHttpServicerFactory}.
	 */
	private static class RawHttpServicerFactory extends AbstractHttpServicerFactory {

		private static HttpHeaderName NAME_SERVER = new HttpHeaderName("Server");

		private static HttpHeaderValue VALUE_SERVER = new HttpHeaderValue("OF");

		private static HttpHeaderName NAME_DATE = new HttpHeaderName("Date");

		private static byte[] HELLO_WORLD = "Hello, World!".getBytes(ServerHttpConnection.DEFAULT_HTTP_ENTITY_CHARSET);

		private static HttpHeaderValue APPLICATION_JSON = new HttpHeaderValue("application/json");

		private static final HttpHeaderValue TEXT_PLAIN = new HttpHeaderValue("text/plain");

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
				break;

			case "/json":
				response.setContentType(APPLICATION_JSON, null);
				this.objectMapper.writeValue(response.getEntityWriter(), new Message("Hello, World!"));
				break;

			default:
				response.setStatus(HttpStatus.NOT_FOUND);
				break;
			}

			try {
				connection.getServiceFlowCallback().run(null);
			} catch (Throwable ex) {
				throw new IOException(ex);
			}
		}
	}

	@Data
	public static class Message {
		private final String message;
	}

}