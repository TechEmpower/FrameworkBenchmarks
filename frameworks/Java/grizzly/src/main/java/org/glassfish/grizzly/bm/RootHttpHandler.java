package org.glassfish.grizzly.bm;

import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.RequestExecutorProvider;
import org.glassfish.grizzly.http.server.Response;
import org.glassfish.grizzly.http.util.DataChunk;

/**
 * Root {@link HttpHandler} to be used to avoid mapping overhead
 */
public class RootHttpHandler extends HttpHandler {
//  Uncomment for real text benchmark
//    private final HttpHandler plainTextHandler = new PlainTextHttpHandler();
    
//  Binary PlainText handler  
    private final HttpHandler plainTextHandler = new PlainText2HttpHandler();
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
    public RequestExecutorProvider getRequestExecutorProvider() {
        return Server.EXECUTOR_PROVIDER;
    }
}
