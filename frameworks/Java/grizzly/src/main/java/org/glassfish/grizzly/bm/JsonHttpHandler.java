package org.glassfish.grizzly.bm;

import com.fasterxml.jackson.databind.*;

import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.RequestExecutorProvider;
import org.glassfish.grizzly.http.server.Response;
import org.glassfish.grizzly.http.util.ContentType;
import org.glassfish.grizzly.http.util.FastHttpDateFormat;
import org.glassfish.grizzly.http.util.Header;

/**
 * JSON test
 */
public class JsonHttpHandler extends HttpHandler {
	private static final ObjectMapper MAPPER = new ObjectMapper();

	private static final ContentType CONTENT_TYPE = ContentType.newContentType("application/json")
			.prepare();

	// Response message class.
	public static class HelloMessage {
		public final String message = "Hello, World!";
	}

	@Override
	public void service(final Request request, final Response response) throws Exception {
		response.setContentType(CONTENT_TYPE);
		response.setHeader(Header.Server, Server.SERVER_VERSION);
		response.setHeader(Header.Date, FastHttpDateFormat.getCurrentDate());

		// Write JSON encoded message to the response.
		MAPPER.writeValue(response.getOutputStream(), new HelloMessage());
	}

	@Override
	public RequestExecutorProvider getRequestExecutorProvider() {
		return Server.EXECUTOR_PROVIDER;
	}
}
