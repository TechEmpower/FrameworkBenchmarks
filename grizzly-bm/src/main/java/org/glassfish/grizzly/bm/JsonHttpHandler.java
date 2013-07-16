package org.glassfish.grizzly.bm;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import java.io.IOException;
import java.util.concurrent.ExecutorService;
import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.Response;
import org.glassfish.grizzly.http.util.Header;

/**
 * Json usecase
 */
public class JsonHttpHandler extends HttpHandler {

    private final JsonFactory factory = new JsonFactory();

    @Override
    public void service(final Request request, final Response response)
            throws Exception {
        response.setContentType("application/json");
        response.setHeader(Header.Server, Server.SERVER_VERSION);

        JsonGenerator generator = null;

        try {
            generator = factory.createGenerator(response.getOutputStream());
            generator.writeStartObject();
            generator.writeStringField("message", "Hello, world");
            generator.writeEndObject();
        } catch (IOException e) {
            throw new IllegalStateException(e);
        } finally {
            if (generator != null) {
                try {
                    generator.close();
                } catch (IOException e) {
                }
            }
        }
    }

    @Override
    protected ExecutorService getThreadPool(Request request) {
        return null;
    }
}
