package benchmarks;

import java.util.concurrent.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;
import net.freeutils.httpserver.HTTPServer;
import net.freeutils.httpserver.HTTPServer.*;

public class Server {

    private static final String HELLO_TEXT = "Hello, World!";
    private static final byte[] HELLO_BYTES = HELLO_TEXT.getBytes();
    private static final String HELLO_LENGTH = Integer.toString(HELLO_BYTES.length);
    private static final ObjectMapper MAPPER = new ObjectMapper();

    static {
        MAPPER.registerModule(new AfterburnerModule());
    }

    private static ContextHandler createPlaintextHandler() {
        return (req, resp) -> {
            resp.getHeaders().add("Content-Type", "text/plain");
            resp.getHeaders().add("Content-Length", HELLO_LENGTH);
            resp.sendHeaders(200);
            resp.getOutputStream().write(HELLO_BYTES);
            return 0;
        };
    }

    private static ContextHandler createJSONHandler() {
        return (req, resp) -> {
            Message msg = new Message(HELLO_TEXT);
            byte[] bytes;
            try {
                bytes = MAPPER.writeValueAsBytes(msg);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }

            resp.getHeaders().add("Content-Type", "application/json");
            resp.getHeaders().add("Content-Length", Integer.toString(bytes.length));
            resp.sendHeaders(200);
            resp.getOutputStream().write(bytes);
            return 0;
        };
    }

    public static void main(String[] args) throws Exception {
        // parse arguments
        int port = args.length > 0 ? Integer.parseInt(args[0]) : 8080;
        // create server
        HTTPServer server = new HTTPServer(port);
        server.setExecutor(new ThreadPoolExecutor(
            8, Integer.MAX_VALUE, 300, TimeUnit.SECONDS, new SynchronousQueue<>()));
        VirtualHost host = server.getVirtualHost(null); // default virtual host
        // add context handlers
        host.addContext("/plaintext", createPlaintextHandler());
        host.addContext("/json", createJSONHandler());
        // start server
        server.start();
    }
}
