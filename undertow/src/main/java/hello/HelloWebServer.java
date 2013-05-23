package hello;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.undertow.UndertowOptions;
import io.undertow.io.IoCallback;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpOpenListener;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;
import org.xnio.BufferAllocator;
import org.xnio.ByteBufferSlicePool;
import org.xnio.ChannelListener;
import org.xnio.ChannelListeners;
import org.xnio.OptionMap;
import org.xnio.Options;
import org.xnio.Pool;
import org.xnio.StreamConnection;
import org.xnio.Xnio;
import org.xnio.XnioWorker;
import org.xnio.channels.AcceptingChannel;

public class HelloWebServer {

    private static final ObjectMapper mapper = new ObjectMapper();

    private final int port;

    public HelloWebServer(int port) {
        this.port = port;
    }

    public void run() throws Exception {

        Xnio xnio = Xnio.getInstance("nio", HelloWebServer.class.getClassLoader());
        XnioWorker worker = xnio.createWorker(OptionMap.builder()
                .set(Options.WORKER_IO_THREADS, Runtime.getRuntime().availableProcessors() * 2)
                .set(Options.CONNECTION_HIGH_WATER, 1000000)
                .set(Options.CONNECTION_LOW_WATER, 1000000)
                .set(Options.TCP_NODELAY, true)
                .set(Options.CORK, true)
                .getMap());

        OptionMap serverOptions = OptionMap.builder()
                .set(Options.TCP_NODELAY, true)
                .set(Options.REUSE_ADDRESSES, true)
                .getMap();

        Pool<ByteBuffer> buffers = new ByteBufferSlicePool(BufferAllocator.DIRECT_BYTE_BUFFER_ALLOCATOR, 2048, 2048 * 2048);

        HttpHandler rootHandler = new HttpHandler() {
            @Override
            public void handleRequest(final HttpServerExchange exchange) throws Exception {
                exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "application/json");
                Map<String, String> data = new HashMap<String, String>();
                data.put("message", "Hello, world");
                String response = mapper.writeValueAsString(data);
                exchange.getResponseHeaders().put(Headers.CONTENT_LENGTH, response.length());
                exchange.getResponseSender().send(response, IoCallback.END_EXCHANGE);
            }
        };

        HttpOpenListener openListener = new HttpOpenListener(buffers, OptionMap.create(UndertowOptions.BUFFER_PIPELINED_DATA, true), 2048);
        openListener.setRootHandler(rootHandler);
        ChannelListener<AcceptingChannel<StreamConnection>> acceptListener = ChannelListeners.openListenerAdapter(openListener);
        AcceptingChannel<? extends StreamConnection> server = worker.createStreamConnectionServer(new InetSocketAddress(InetAddress.getByAddress(new byte[]{0, 0, 0, 0}), port), acceptListener, serverOptions);
        server.resumeAccepts();


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