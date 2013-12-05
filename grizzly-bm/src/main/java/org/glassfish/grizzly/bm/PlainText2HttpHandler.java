package org.glassfish.grizzly.bm;

import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.RequestExecutorProvider;
import org.glassfish.grizzly.http.server.Response;
import org.glassfish.grizzly.http.util.ContentType;
import org.glassfish.grizzly.http.util.Header;
import org.glassfish.grizzly.utils.Charsets;

/**
 * Binary version of plain text usecase
 * 
 * @see PlainTextHttpHandler
 */
public class PlainText2HttpHandler extends HttpHandler {
    private static final ContentType CONTENT_TYPE =
            ContentType.newContentType("text/plain", "utf-8").prepare();
    private static final byte[] HELLO_WORLD_BYTES = "Hello, World!".getBytes(Charsets.UTF8_CHARSET);
            
    @Override
    public void service(final Request request, final Response response)
            throws Exception {
        response.setContentType(CONTENT_TYPE);
        response.setHeader(Header.Server, Server.SERVER_VERSION);
        response.getOutputStream().write(HELLO_WORLD_BYTES);
    }

    @Override
    public RequestExecutorProvider getRequestExecutorProvider() {
        return Server.EXECUTOR_PROVIDER;
    }
}
