package org.glassfish.grizzly.bm;

import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.RequestExecutorProvider;
import org.glassfish.grizzly.http.server.Response;
import org.glassfish.grizzly.http.util.Header;

/**
 * Plain text usecase
 */
public class PlainTextHttpHandler extends HttpHandler {

    @Override
    public void service(final Request request, final Response response)
            throws Exception {
        response.setContentType("text/plain");
        response.setCharacterEncoding("UTF-8");
        response.setHeader(Header.Server, Server.SERVER_VERSION);
        response.getWriter().write("Hello, World!");
    }

    @Override
    public RequestExecutorProvider getRequestExecutorProvider() {
        return Server.EXECUTOR_PROVIDER;
    }
}
