package hello.webactors;

import co.paralleluniverse.comsat.webactors.netty.AutoWebActorHandler;
import co.paralleluniverse.embedded.containers.AbstractEmbeddedServer;
import hello.Server;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.PooledByteBufAllocator;
import io.netty.channel.*;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.codec.http.HttpRequestDecoder;
import io.netty.handler.codec.http.HttpResponseEncoder;

import java.util.Map;
import java.util.concurrent.Callable;

public final class NettyServer implements Server {
    @Override
    public final void run() throws Exception {
        WebActor.SERVER = "comsat-webactors-netty";
        final ServerBootstrap b = new ServerBootstrap();

        b.option(ChannelOption.SO_BACKLOG, 100000);
        b.childOption(ChannelOption.CONNECT_TIMEOUT_MILLIS, 0);
        b.childOption(ChannelOption.TCP_NODELAY, true);
        b.childOption(ChannelOption.SO_REUSEADDR, true);
        b.childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT);

        final ChannelInitializer<SocketChannel> childHandler = new SocketChannelChannelInitializer(() -> new AutoWebActorHandler() {
            @Override
            protected AutoContextProvider newContextProvider(ClassLoader userClassLoader, Map<Class<?>, Object[]> actorParams) {
                return new AutoContextProvider(userClassLoader, actorParams, 1_000_000L /* ms */);
            }
        });
        final NioEventLoopGroup group = new NioEventLoopGroup(200);
        b.group(group)
            .channel(NioServerSocketChannel.class)
            .childHandler(childHandler);

        final ChannelFuture cf = b.bind(8080);
        cf.sync();

        System.err.println("Server is up.");

        AbstractEmbeddedServer.waitUrlAvailable("http://localhost:8080/plaintext");
        AbstractEmbeddedServer.waitUrlAvailable("http://localhost:8080/json");
        System.err.println("Server test cases are instrumented and bootstrapped.");
    }

    private static final class SocketChannelChannelInitializer extends ChannelInitializer<SocketChannel> {
        private final Callable<SimpleChannelInboundHandler<Object>> handlerProvider;

        public SocketChannelChannelInitializer(Callable<SimpleChannelInboundHandler<Object>> handlerProvider) {
            this.handlerProvider = handlerProvider;
        }

        @Override
        public final void initChannel(SocketChannel ch) throws Exception {
            final ChannelPipeline pipeline = ch.pipeline();
            pipeline.addLast(new HttpRequestDecoder());
            pipeline.addLast(new HttpResponseEncoder());
            pipeline.addLast(new HttpObjectAggregator(65536));
            pipeline.addLast(handlerProvider.call());
        }
    }
}
