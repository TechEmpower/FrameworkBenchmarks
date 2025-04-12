package hello;

import java.util.concurrent.ScheduledExecutorService;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.socket.SocketChannel;
import io.netty.handler.codec.http.*;

public class HelloServerInitializer extends ChannelInitializer<SocketChannel> {

	public HelloServerInitializer() {
	}

	@Override
	public void initChannel(SocketChannel ch) throws Exception {
		ch.pipeline()
                .addLast("encoder", new HttpResponseEncoder() {

					private ByteBuf encodedHeaders;
					private HttpHeaders lastSeenHeaders;

					@Override
					protected void encodeHeaders(HttpHeaders headers, ByteBuf buf) {
						if (lastSeenHeaders != headers) {
							updateEncodedHttpHeaders(headers, buf);
						}
						encodedHeaders.getBytes(encodedHeaders.readerIndex(), buf, encodedHeaders.readableBytes());
					}

					private void updateEncodedHttpHeaders(HttpHeaders headers, ByteBuf buf) {
						if (encodedHeaders == null) {
							encodedHeaders = Unpooled.buffer(buf.writableBytes());
						} else {
							encodedHeaders.clear().ensureWritable(buf.writableBytes());
						}
						super.encodeHeaders(headers, encodedHeaders);
						lastSeenHeaders = headers;
					}

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
                .addLast("handler", newHelloServerHandler(ch.eventLoop()));
	}

	protected HelloServerHandler newHelloServerHandler(ScheduledExecutorService service) {
		return new HelloServerHandler(service);
	}
}
