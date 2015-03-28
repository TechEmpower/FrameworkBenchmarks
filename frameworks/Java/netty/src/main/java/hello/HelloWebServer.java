package hello;

import java.net.InetSocketAddress;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.PooledByteBufAllocator;
import io.netty.channel.Channel;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.ServerChannel;
import io.netty.channel.epoll.Epoll;
import io.netty.channel.epoll.EpollEventLoopGroup;
import io.netty.channel.epoll.EpollServerSocketChannel;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.util.ResourceLeakDetector;
import io.netty.util.ResourceLeakDetector.Level;

public class HelloWebServer {

    static {
	ResourceLeakDetector.setLevel(Level.DISABLED);
    }

    private final int port;

    public HelloWebServer(int port) {
	this.port = port;
    }

    public void run() throws Exception {
	// Configure the server.

	if (Epoll.isAvailable()) {
	    doRun(new EpollEventLoopGroup(), EpollServerSocketChannel.class);
	} else {
	    doRun(new NioEventLoopGroup(), NioServerSocketChannel.class);
	}
    }

    private void doRun(EventLoopGroup loupGroup, Class<? extends ServerChannel> serverChannelClass) throws InterruptedException {
	try {
		InetSocketAddress inet = new InetSocketAddress(port);
		
	    ServerBootstrap b = new ServerBootstrap();
	    b.option(ChannelOption.SO_BACKLOG, 1024);
	    b.option(ChannelOption.SO_REUSEADDR, true);
	    b.group(loupGroup).channel(serverChannelClass).childHandler(new HelloServerInitializer(loupGroup.next()));
	    b.option(ChannelOption.MAX_MESSAGES_PER_READ, Integer.MAX_VALUE);
	    b.childOption(ChannelOption.ALLOCATOR, new PooledByteBufAllocator(true));
	    b.childOption(ChannelOption.SO_REUSEADDR, true);
	    b.childOption(ChannelOption.MAX_MESSAGES_PER_READ, Integer.MAX_VALUE);

	    Channel ch = b.bind(inet).sync().channel();
	    
	    System.out.printf("Httpd started. Listening on: %s%n", inet.toString());	    
	    
	    ch.closeFuture().sync();
	} finally {
	    loupGroup.shutdownGracefully().sync();
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
