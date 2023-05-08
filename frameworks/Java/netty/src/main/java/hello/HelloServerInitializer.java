package hello;

import java.util.concurrent.ScheduledExecutorService;

import io.netty.channel.ChannelInitializer;
import io.netty.channel.socket.SocketChannel;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.DefaultHttpRequest;
import io.netty.handler.codec.http.HttpMessage;
import io.netty.handler.codec.http.HttpMethod;
import io.netty.handler.codec.http.HttpRequestDecoder;
import io.netty.handler.codec.http.HttpResponseEncoder;
import io.netty.handler.codec.http.HttpVersion;

public class HelloServerInitializer extends ChannelInitializer<SocketChannel> {

	private final ScheduledExecutorService service;

	public HelloServerInitializer(ScheduledExecutorService service) {
		this.service = service;
	}

	@Override
	public void initChannel(SocketChannel ch) throws Exception {
		ch.pipeline()
                .addLast("encoder", new HttpResponseEncoder() {
					@Override
					public boolean acceptOutboundMessage(final Object msg) throws Exception {
						if (msg.getClass() == DefaultFullHttpResponse.class) {
							return true;
						}
						return super.acceptOutboundMessage(msg);
					}
				})
                .addLast("decoder", new HttpRequestDecoder(4096, 8192, 8192, false) {

					@Override
					protected HttpMessage createMessage(final String[] initialLine) throws Exception {
						return new DefaultHttpRequest(
								HttpVersion.valueOf(initialLine[2]),
								HttpMethod.valueOf(initialLine[0]), initialLine[1], validateHeaders);
					}

					@Override
					protected boolean isContentAlwaysEmpty(final HttpMessage msg) {
						return false;
					}
				})
                .addLast("handler", new HelloServerHandler(service));
	}
}
