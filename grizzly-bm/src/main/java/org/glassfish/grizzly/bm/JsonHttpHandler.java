package org.glassfish.grizzly.bm;

import com.fasterxml.jackson.databind.*;
import java.io.IOException;
import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.RequestExecutorProvider;
import org.glassfish.grizzly.http.server.Response;
import org.glassfish.grizzly.http.util.Header;

/**
 * Json usecase
 */
public class JsonHttpHandler extends HttpHandler {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    
    // Response message class.
    public static class HelloMessage {
      public final String message = "Hello, World!";
    }

    @Override
    public void service(final Request request, final Response response)
            throws Exception {
        response.setContentType("application/json");
        response.setHeader(Header.Server, Server.SERVER_VERSION);

        // Write JSON encoded message to the response.
        try
        {
          MAPPER.writeValue(response.getOutputStream(), new HelloMessage());
        }
        catch (IOException ioe) 
        {
          // do nothing
        }
    }

    @Override
    public RequestExecutorProvider getRequestExecutorProvider() {
        return Server.EXECUTOR_PROVIDER;
    }
}
