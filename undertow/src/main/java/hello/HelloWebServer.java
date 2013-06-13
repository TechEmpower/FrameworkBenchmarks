package hello;

import java.nio.ByteBuffer;
import java.util.Collections;
import java.util.Map;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import io.undertow.Undertow;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

import static io.undertow.Undertow.builder;

public class HelloWebServer {

    private static final ObjectWriter writer = new ObjectMapper().writerWithType(Map.class);

    public static final String HELLO_WORLD = "Hello, World!";

    private final int port;
    private final ByteBuffer buffer;

    public HelloWebServer(int port) {
        this.port = port;
        buffer = ByteBuffer.allocateDirect(HELLO_WORLD.getBytes().length);
        buffer.put(HELLO_WORLD.getBytes());
        buffer.flip();
    }

    public void run() throws Exception {

        Undertow undertow = builder()
                .addListener(port, "0.0.0.0")
                .setBufferSize(1024 * 16)
                .setHandler(new HttpHandler() {
                    @Override
                    public void handleRequest(final HttpServerExchange exchange) throws Exception {
                        if (exchange.getRelativePath().equals("/plaintext")) {
                            exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "text/plain");
                            exchange.getResponseSender().send(buffer.duplicate());
                        } else {
                            exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "application/json");
                            Map<String, String> data = Collections.singletonMap("message", "Hello, world");
                            String response = writer.writeValueAsString(data);
                            exchange.getResponseSender().send(response);
                        }
                    }
                }).build();

        undertow.start();
    }

    public static void main(String[] args) throws Exception {
        int port;
        if (args.length > 0) {
            port = Integer.parseInt(args[0]);
        } else {
            port = 8080;
        }
        new HelloWebServer(port).run();
    }
}