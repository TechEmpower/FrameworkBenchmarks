package hello;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.Channel;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;

public class HelloWebServer {

    private final int port;

    public HelloWebServer(int port) {
        this.port = port;
    }

    public void run() throws Exception {
        // Configure the server.
        ServerBootstrap b = new ServerBootstrap();

        try {
            b.group(new NioEventLoopGroup(), new NioEventLoopGroup())
             .channel(NioServerSocketChannel.class)
             .childHandler(new HelloServerInitializer());

            Channel ch = b.bind(port).sync().channel();
            ch.closeFuture().sync();
        } finally {
            b.shutdown();
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