package org.glassfish.grizzly.bm;

import java.util.concurrent.ExecutorService;
import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.Response;
import org.glassfish.grizzly.http.util.DataChunk;

/**
 * Root {@link HttpHandler} to be used to avoid mapping overhead
 */
public class RootHttpHandler extends HttpHandler {
    private final HttpHandler plainTextHandler = new PlainTextHttpHandler();
    private final HttpHandler jsonHandler = new JsonHttpHandler();
    
    @Override
    public void service(final Request request, final Response response)
            throws Exception {
        // don't decode and avoid creating a string
        final DataChunk requestURIBC = request.getRequest()
                .getRequestURIRef().getRequestURIBC();
        
        if (requestURIBC.equals("/json")) {
            jsonHandler.service(request, response);
        } else {
            plainTextHandler.service(request, response);
        }
    }
    
    @Override
    protected ExecutorService getThreadPool(Request request) {
        return null;
    }
}
