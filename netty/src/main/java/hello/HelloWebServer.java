package hello;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.PooledByteBufAllocator;
import io.netty.channel.Channel;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.util.ResourceLeakDetector;


public class HelloWebServer {
    private static int IO_THREADS = Runtime.getRuntime().availableProcessors() * 2;
    static {
        ResourceLeakDetector.setLevel(ResourceLeakDetector.Level.DISABLED);
    }

    private final int port;

    public HelloWebServer(int port) {
        this.port = port;
    }

    public void run() throws Exception {
        // Configure the server.
        EventLoopGroup group = new NioEventLoopGroup(IO_THREADS);
        try {
            ServerBootstrap b = new ServerBootstrap();
            b.group(group)
             .childHandler(new HelloServerInitializer(group.next()))
             .channel(NioServerSocketChannel.class)
             .option(ChannelOption.SO_BACKLOG, 1024)
             .option(ChannelOption.SO_REUSEADDR, true)
             .option(ChannelOption.MAX_MESSAGES_PER_READ, Integer.MAX_VALUE)
             .childOption(ChannelOption.ALLOCATOR, new PooledByteBufAllocator(true))
             .childOption(ChannelOption.SO_REUSEADDR, true)
             .childOption(ChannelOption.MAX_MESSAGES_PER_READ, Integer.MAX_VALUE);
             
            Channel ch = b.bind(port).sync().channel();
            ch.closeFuture().sync();
        } finally {
            group.shutdownGracefully().sync();
        }
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
