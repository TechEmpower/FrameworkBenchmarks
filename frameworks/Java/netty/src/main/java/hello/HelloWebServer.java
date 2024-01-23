package hello;

import java.net.InetSocketAddress;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.Channel;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.ServerChannel;
import io.netty.channel.epoll.Epoll;
import io.netty.channel.epoll.EpollChannelOption;
import io.netty.channel.epoll.EpollEventLoopGroup;
import io.netty.channel.epoll.EpollServerSocketChannel;
import io.netty.channel.kqueue.KQueue;
import io.netty.channel.kqueue.KQueueServerSocketChannel;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.incubator.channel.uring.IOUring;
import io.netty.incubator.channel.uring.IOUringChannelOption;
import io.netty.incubator.channel.uring.IOUringEventLoopGroup;
import io.netty.incubator.channel.uring.IOUringServerSocketChannel;
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
		if (IOUring.isAvailable()) {
			doRun(new IOUringEventLoopGroup(Runtime.getRuntime().availableProcessors()), IOUringServerSocketChannel.class, IoMultiplexer.IO_URING);
		} else
			if (Epoll.isAvailable()) {
			doRun(new EpollEventLoopGroup(), EpollServerSocketChannel.class, IoMultiplexer.EPOLL);
		} else if (KQueue.isAvailable()) {
			doRun(new EpollEventLoopGroup(), KQueueServerSocketChannel.class, IoMultiplexer.KQUEUE);
		} else {
			doRun(new NioEventLoopGroup(), NioServerSocketChannel.class, IoMultiplexer.JDK);
		}
	}

	private void doRun(EventLoopGroup loupGroup, Class<? extends ServerChannel> serverChannelClass, IoMultiplexer multiplexer) throws InterruptedException {
		try {
			InetSocketAddress inet = new InetSocketAddress(port);
			
			System.out.printf("Using %s IoMultiplexer%n", multiplexer);

			ServerBootstrap b = new ServerBootstrap();

			if (multiplexer == IoMultiplexer.EPOLL) {
				b.option(EpollChannelOption.SO_REUSEPORT, true);
			}
			
			if (multiplexer == IoMultiplexer.IO_URING) {
				b.option(IOUringChannelOption.SO_REUSEPORT, true);
			}
			
			b.option(ChannelOption.SO_BACKLOG, 8192);
			b.option(ChannelOption.SO_REUSEADDR, true);
			b.group(loupGroup).channel(serverChannelClass).childHandler(new HelloServerInitializer(loupGroup.next()));
			b.childOption(ChannelOption.SO_REUSEADDR, true);

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
