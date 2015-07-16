package servers;

import co.paralleluniverse.comsat.webactors.netty.AutoWebActorHandler;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.PooledByteBufAllocator;
import io.netty.channel.*;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.codec.http.HttpRequestDecoder;
import io.netty.handler.codec.http.HttpResponseEncoder;

public class NettyServer implements Server {
	private NioEventLoopGroup group = new NioEventLoopGroup();
	private ChannelFuture ch;

	@Override
	public void start() throws Exception {
		final ServerBootstrap b = new ServerBootstrap();
		b.option(ChannelOption.SO_BACKLOG, 65535);
		b.childOption(ChannelOption.CONNECT_TIMEOUT_MILLIS, 0);
		b.childOption(ChannelOption.TCP_NODELAY, true);
		b.childOption(ChannelOption.SO_REUSEADDR, true);
		b.childOption(ChannelOption.SO_LINGER, 0);
		b.childOption(ChannelOption.WRITE_BUFFER_HIGH_WATER_MARK, 32 * 1024);
		b.childOption(ChannelOption.WRITE_BUFFER_LOW_WATER_MARK, 8 * 1024);
		b.childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT);

		final ChannelInitializer<SocketChannel> childHandler = new SocketChannelChannelInitializer();
		b.group(group)
			.channel(NioServerSocketChannel.class)
			.childHandler(childHandler);

		ch = b.bind(8080).sync();

		System.err.println("Server is up.");
	}

	@Override
	public void stop() throws Exception {
		if (ch != null && ch.channel().isOpen())
			ch.channel().close();
		if (group != null)
			group.shutdownGracefully().sync();

		System.err.println("Server is down.");
	}

	private static class SocketChannelChannelInitializer extends ChannelInitializer<SocketChannel> {
		@Override
		public void initChannel(SocketChannel ch) throws Exception {
			final ChannelPipeline pipeline = ch.pipeline();
			pipeline.addLast(new HttpRequestDecoder());
			pipeline.addLast(new HttpResponseEncoder());
			pipeline.addLast(new HttpObjectAggregator(65536));
			pipeline.addLast(new AutoWebActorHandler());
		}
	}
}
