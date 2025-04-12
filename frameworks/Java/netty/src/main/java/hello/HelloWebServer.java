package hello;

import java.net.InetSocketAddress;

import hello.loom.HelloLoomServerInitializer;
import hello.loom.LoomSupport;
import hello.loom.MultithreadVirtualEventExecutorGroup;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.Channel;
import io.netty.channel.ChannelOption;
import io.netty.channel.epoll.EpollChannelOption;
import io.netty.channel.uring.IoUringChannelOption;
import io.netty.util.ResourceLeakDetector;
import io.netty.util.ResourceLeakDetector.Level;

public class HelloWebServer {

	private static final boolean EVENT_LOOP_CARRIER = Boolean.getBoolean("hello.eventloop.carrier");
	private static final IoMultiplexer PREFERRED_TRANSPORT;

	static {
		ResourceLeakDetector.setLevel(Level.DISABLED);
		String transportName = System.getProperty("hello.transport");
		if (transportName != null) {
			try {
				PREFERRED_TRANSPORT = IoMultiplexer.valueOf(transportName);
			} catch (IllegalArgumentException e) {
				System.err.println("Invalid transport name: " + transportName);
				throw e;
			}
		} else {
			PREFERRED_TRANSPORT = IoMultiplexer.type();
		}
	}

	private final int port;

	public HelloWebServer(int port) {
		this.port = port;
	}

	public void run() throws Exception {
		final var preferredTransport = PREFERRED_TRANSPORT;
		System.out.printf("Using %s IoMultiplexer%n", preferredTransport);
		final int coreCount = Runtime.getRuntime().availableProcessors();
		final var group = EVENT_LOOP_CARRIER?
				preferredTransport.newVirtualEventExecutorGroup(coreCount) :
				preferredTransport.newEventLoopGroup(coreCount);
		if (EVENT_LOOP_CARRIER) {
			LoomSupport.checkSupported();
			System.out.println("Using EventLoop optimized for Loom");
		}
		try {
			final var serverChannelClass = preferredTransport.serverChannelClass();
			var inet = new InetSocketAddress(port);
			var b = new ServerBootstrap();

			b.option(ChannelOption.SO_BACKLOG, 8192);
			b.option(ChannelOption.SO_REUSEADDR, true);
			switch (preferredTransport) {
				case EPOLL:
					b.option(EpollChannelOption.SO_REUSEPORT, true);
					break;
				case IO_URING:
					b.option(IoUringChannelOption.SO_REUSEPORT, true);
					break;
			}
			var channelB = b.group(group).channel(serverChannelClass);
			if (EVENT_LOOP_CARRIER) {
				channelB.childHandler(new HelloLoomServerInitializer((MultithreadVirtualEventExecutorGroup) group, group.next()));
			} else {
				channelB.childHandler(new HelloServerInitializer(group.next()));
			}
			b.childOption(ChannelOption.SO_REUSEADDR, true);

			Channel ch = b.bind(inet).sync().channel();

			System.out.printf("Httpd started. Listening on: %s%n", inet);

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
