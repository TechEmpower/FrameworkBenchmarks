package org.glassfish.grizzly.bm;

import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.RequestExecutorProvider;
import org.glassfish.grizzly.http.server.Response;
import org.glassfish.grizzly.http.util.ContentType;
import org.glassfish.grizzly.http.util.FastHttpDateFormat;
import org.glassfish.grizzly.http.util.Header;

/**
 * Plaintext test case
 */
public class PlainTextHttpHandler extends HttpHandler {
	private static final ContentType CONTENT_TYPE = ContentType.newContentType("text/plain")
			.prepare();

	@Override
	public void service(final Request request, final Response response) throws Exception {
		response.setContentType(CONTENT_TYPE);
		response.setHeader(Header.Server, Server.SERVER_VERSION);
		response.setHeader(Header.Date, FastHttpDateFormat.getCurrentDate());
		response.getWriter().write("Hello, World!");
	}

	@Override
	public RequestExecutorProvider getRequestExecutorProvider() {
		return Server.EXECUTOR_PROVIDER;
	}
}
