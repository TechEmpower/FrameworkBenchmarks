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
			doRun(new EpollEventLoopGroup(), EpollServerSocketChannel.class, true);
		} else {
			doRun(new NioEventLoopGroup(), NioServerSocketChannel.class, false);
		}
	}

	private void doRun(EventLoopGroup loupGroup, Class<? extends ServerChannel> serverChannelClass, boolean isNative) throws InterruptedException {
		try {
			InetSocketAddress inet = new InetSocketAddress(port);

			ServerBootstrap b = new ServerBootstrap();

			if (isNative) {
				b.option(EpollChannelOption.SO_REUSEPORT, true);
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
