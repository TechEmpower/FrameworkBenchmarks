package hello.webactors;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.comsat.webactors.HttpRequest;
import co.paralleluniverse.comsat.webactors.HttpResponse;

import co.paralleluniverse.fibers.SuspendExecution;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.nio.ByteBuffer;

import static co.paralleluniverse.comsat.webactors.HttpResponse.error;
import static co.paralleluniverse.comsat.webactors.HttpResponse.ok;

@co.paralleluniverse.comsat.webactors.WebActor(httpUrlPatterns = {"/*"})
public final class WebActor extends BasicActor<Object, Void> {
    static String SERVER = "comsat-webactors";

    private static final String HELLO_WORLD = "Hello, World!";
    private static final byte[] HELLO_WORLD_A = HELLO_WORLD.getBytes();

    private static final class HelloWorldData {
        @SuppressWarnings("unused")
        public final String message = HELLO_WORLD;
    }

    private static final ObjectMapper mapper = new ObjectMapper();

    @Override
    protected final Void doRun() throws InterruptedException, SuspendExecution {
        final Object message = receive();
        if (message instanceof HttpRequest) {
            final HttpRequest req = (HttpRequest) message;
            HttpResponse.Builder res;
            final String s = req.getRequestURI();
            if ("/plaintext".equals(s)) {
                final ByteBuffer b = ByteBuffer.wrap(HELLO_WORLD_A);
                res = ok(self(), req, b).setContentType("text/plain");
            } else if ("/json".equals(s)) {
                try {
                    res = ok(self(), req, mapper.writeValueAsString(new HelloWorldData())).setContentType("application/json");
                } catch (final JsonProcessingException e) {
                    throw new RuntimeException(e);
                }
            } else {
                res = error(self(), req, 404, "Not found");
            }
            req.getFrom().send (
                res
                    .addHeader("Server", SERVER)
                    .build()
            );
        }
        return null;
    }
}
