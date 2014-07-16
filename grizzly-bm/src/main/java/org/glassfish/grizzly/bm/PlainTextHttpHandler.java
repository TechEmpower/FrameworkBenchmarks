package org.glassfish.grizzly.bm;

import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.RequestExecutorProvider;
import org.glassfish.grizzly.http.server.Response;
import org.glassfish.grizzly.http.util.ContentType;
import org.glassfish.grizzly.http.util.Header;

/**
 * Plain text usecase
 */
public class PlainTextHttpHandler extends HttpHandler {
    private static final ContentType CONTENT_TYPE =
            ContentType.newContentType("text/plain", "utf-8").prepare();

    @Override
    public void service(final Request request, final Response response)
            throws Exception {
        response.setContentType(CONTENT_TYPE);
        response.setHeader(Header.Server, Server.SERVER_VERSION);
        response.getWriter().write("Hello, World!");
    }

    @Override
    public RequestExecutorProvider getRequestExecutorProvider() {
        return Server.EXECUTOR_PROVIDER;
    }
}
