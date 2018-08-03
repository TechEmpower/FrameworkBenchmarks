package hello;

import java.util.concurrent.ScheduledExecutorService;

import io.netty.channel.ChannelInitializer;
import io.netty.channel.socket.SocketChannel;
import io.netty.handler.codec.http.HttpRequestDecoder;
import io.netty.handler.codec.http.HttpResponseEncoder;

public class HelloServerInitializer extends ChannelInitializer<SocketChannel> {

	private ScheduledExecutorService service;

	public HelloServerInitializer(ScheduledExecutorService service) {
		this.service = service;
	}

	@Override
	public void initChannel(SocketChannel ch) throws Exception {
		ch.pipeline()
                .addLast("encoder", new HttpResponseEncoder())
                .addLast("decoder", new HttpRequestDecoder(4096, 8192, 8192, false))
                .addLast("handler", new HelloServerHandler(service));
	}
}
