package benchmarks;

import java.io.ByteArrayInputStream;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;
import fi.iki.elonen.NanoHTTPD;

public class Server extends NanoHTTPD {

    private static final String SERVER_NAME = "NanoHTTPD";
    private static final String HELLO_TEXT = "Hello, World!";
    private static final byte[] HELLO_BYTES = HELLO_TEXT.getBytes();
    private static final int HELLO_LENGTH = HELLO_BYTES.length;
    private static final ObjectMapper MAPPER = new ObjectMapper();

    static {
        MAPPER.registerModule(new AfterburnerModule());
    }

    public Server(int port) {
        super(port);
    }

    @Override public Response serve(IHTTPSession session) {
        Response response;
        switch (session.getUri()) {

            case "/plaintext":
                response = newFixedLengthResponse(Response.Status.OK, "text/plain",
                    new ByteArrayInputStream(HELLO_BYTES), HELLO_LENGTH);
                response.addHeader("Server", SERVER_NAME);
                return response;

            case "/json":
                Message msg = new Message(HELLO_TEXT);
                byte[] bytes;
                try {
                    bytes = MAPPER.writeValueAsBytes(msg);
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
                response = newFixedLengthResponse(Response.Status.OK, "application/json",
                    new ByteArrayInputStream(bytes), bytes.length);
                response.addHeader("Server", SERVER_NAME);
                return response;

            default:
                return newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "Not found");
        }
    }

    public static void main(String[] args) throws Exception {
        int port = args.length > 0 ? Integer.parseInt(args[0]) : 8080;
        NanoHTTPD server = new Server(port);
        server.start(NanoHTTPD.SOCKET_READ_TIMEOUT, false);
    }
}
