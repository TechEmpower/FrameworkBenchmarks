package hello;

import co.paralleluniverse.actors.Actor;
import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.comsat.webactors.HttpRequest;
import co.paralleluniverse.comsat.webactors.HttpResponse;

import co.paralleluniverse.comsat.webactors.WebMessage;
import co.paralleluniverse.comsat.webactors.netty.WebActorHandler;
import co.paralleluniverse.embedded.containers.AbstractEmbeddedServer;
import co.paralleluniverse.fibers.SuspendExecution;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.PooledByteBufAllocator;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelOption;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.codec.http.FullHttpRequest;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.codec.http.HttpRequestDecoder;
import io.netty.handler.codec.http.HttpResponseEncoder;

import java.nio.ByteBuffer;
// import java.text.SimpleDateFormat;
// import java.util.Locale;
// import java.util.TimeZone;

import static co.paralleluniverse.comsat.webactors.HttpResponse.error;
import static co.paralleluniverse.comsat.webactors.HttpResponse.ok;

public final class HelloWebActor extends BasicActor<Object, Void> {
    /////////////////// WEB ACTOR PART ///////////////////

    private static final String HELLO_WORLD = "Hello, World!";
    private static final byte[] HELLO_WORLD_A = HELLO_WORLD.getBytes();

    private static final class HelloWorldData {
        @SuppressWarnings("unused")
        public final String message = HELLO_WORLD;
    }

    private static final ObjectMapper mapper = new ObjectMapper();

//    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US);
//    static {
//        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
//    }

    @Override
    protected final Void doRun() throws InterruptedException, SuspendExecution {
        //noinspection InfiniteLoopStatement
        for (; ; ) {
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
                req.getFrom().send(
                    res
                        .addHeader("Server", "comsat-webactors")
//                        .addHeader("Date", dateFormat.format(calendar.getTime()))
                        .build()
                );
            }
        }
    }

    /////////////////// SERVER PART ///////////////////

    private static final Actor actor = new HelloWebActor();
    @SuppressWarnings("unchecked")
    private static final ActorRef<? extends WebMessage> actorRef = actor.spawn();

    private static final WebActorHandler.DefaultContextImpl context = new MyDefaultContextImpl();

    private static class SocketChannelChannelInitializer extends ChannelInitializer<SocketChannel> {
        @Override
        public void initChannel(SocketChannel ch) throws Exception {
            final ChannelPipeline pipeline = ch.pipeline();
            pipeline.addLast(new HttpRequestDecoder());
            pipeline.addLast(new HttpResponseEncoder());
            pipeline.addLast(new HttpObjectAggregator(65536));
            pipeline.addLast(new WebActorHandler(new MyWebActorContextProvider()));
        }

        private static class MyWebActorContextProvider implements WebActorHandler.WebActorContextProvider {
            @Override
            public WebActorHandler.Context get(ChannelHandlerContext ctx, FullHttpRequest req) {
                return context;
            }
        }
    }

    private static class MyDefaultContextImpl extends WebActorHandler.DefaultContextImpl {
        @SuppressWarnings("unchecked")
        @Override
        public ActorRef<? extends WebMessage> getRef() {
            return actorRef;
        }

        @Override
        public final boolean handlesWithWebSocket(String uri) {
            return false;
        }

        @Override
        public final boolean handlesWithHttp(String uri) {
            return true;
        }

        @Override
        public final boolean watch() {
            return false;
        }
    }

    public static void main(String[] args) throws Exception {
        final ServerBootstrap b = new ServerBootstrap();

        b.option(ChannelOption.SO_BACKLOG, 65535);
        b.childOption(ChannelOption.CONNECT_TIMEOUT_MILLIS, 0);
        b.childOption(ChannelOption.TCP_NODELAY, true);
        b.childOption(ChannelOption.SO_REUSEADDR, true);
        b.childOption(ChannelOption.SO_LINGER, 0);
        // b.childOption(ChannelOption.WRITE_BUFFER_HIGH_WATER_MARK, 32 * 1024);
        // b.childOption(ChannelOption.WRITE_BUFFER_LOW_WATER_MARK, 8 * 1024);
        b.childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT);

        final ChannelInitializer<SocketChannel> childHandler = new SocketChannelChannelInitializer();
        final NioEventLoopGroup group = new NioEventLoopGroup();
        b.group(group)
            .channel(NioServerSocketChannel.class)
            .childHandler(childHandler);

        b.bind(8080).sync();
        System.err.println("Server is up.");
        AbstractEmbeddedServer.waitUrlAvailable("http://localhost:8080/plaintext");
        AbstractEmbeddedServer.waitUrlAvailable("http://localhost:8080/json");
        System.err.println("Server test cases are instrumented and bootstrapped.");
    }
}
