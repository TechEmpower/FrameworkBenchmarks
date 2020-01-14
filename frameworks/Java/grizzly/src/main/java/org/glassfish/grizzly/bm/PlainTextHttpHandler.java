package org.glassfish.grizzly.bm;

import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.RequestExecutorProvider;
import org.glassfish.grizzly.http.server.Response;
import org.glassfish.grizzly.http.util.ContentType;

/**
 * Plaintext test case
 */
public class PlainTextHttpHandler extends HttpHandler {
	private static final ContentType CONTENT_TYPE = ContentType.newContentType("text/plain")
			.prepare();

	@Override
	public void service(final Request request, final Response response) throws Exception {
		response.setContentType(CONTENT_TYPE);
		response.getWriter().write("Hello, World!");
	}

	@Override
	public RequestExecutorProvider getRequestExecutorProvider() {
		return Server.EXECUTOR_PROVIDER;
	}
}
