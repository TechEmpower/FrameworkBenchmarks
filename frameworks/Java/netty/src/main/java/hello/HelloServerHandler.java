package hello;

import static io.netty.handler.codec.http.HttpHeaderNames.CONTENT_LENGTH;
import static io.netty.handler.codec.http.HttpHeaderNames.CONTENT_TYPE;
import static io.netty.handler.codec.http.HttpHeaderNames.DATE;
import static io.netty.handler.codec.http.HttpHeaderNames.SERVER;
import static io.netty.handler.codec.http.HttpHeaderValues.APPLICATION_JSON;
import static io.netty.handler.codec.http.HttpHeaderValues.TEXT_PLAIN;
import static io.netty.handler.codec.http.HttpResponseStatus.NOT_FOUND;
import static io.netty.handler.codec.http.HttpResponseStatus.OK;
import static io.netty.handler.codec.http.HttpVersion.HTTP_1_1;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import com.jsoniter.output.JsonStream;
import com.jsoniter.output.JsonStreamPool;
import com.jsoniter.spi.JsonException;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.DefaultHttpRequest;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.LastHttpContent;
import io.netty.util.AsciiString;
import io.netty.util.CharsetUtil;
import io.netty.util.ReferenceCountUtil;
import io.netty.util.concurrent.FastThreadLocal;

public class HelloServerHandler extends ChannelInboundHandlerAdapter {

	private static final FastThreadLocal<DateFormat> FORMAT = new FastThreadLocal<DateFormat>() {
		@Override
		protected DateFormat initialValue() {
			return new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z");
		}
	};

	private static Message newMsg() {
		return new Message("Hello, World!");
	}

	private static byte[] serializeMsg(Message obj) {
		JsonStream stream = JsonStreamPool.borrowJsonStream();
		try {
			stream.reset(null);
			stream.writeVal(Message.class, obj);
			return Arrays.copyOfRange(stream.buffer().data(), 0, stream.buffer().tail());
		} catch (IOException e) {
			throw new JsonException(e);
		} finally {
			JsonStreamPool.returnJsonStream(stream);
		}
	}

	private static int jsonLen() {
		return serializeMsg(newMsg()).length;
	}

	private static final byte[] STATIC_PLAINTEXT = "Hello, World!".getBytes(CharsetUtil.UTF_8);
	private static final int STATIC_PLAINTEXT_LEN = STATIC_PLAINTEXT.length;

	private static final CharSequence PLAINTEXT_CLHEADER_VALUE = AsciiString.cached(String.valueOf(STATIC_PLAINTEXT_LEN));
	private static final int JSON_LEN = jsonLen();
	private static final CharSequence JSON_CLHEADER_VALUE = AsciiString.cached(String.valueOf(JSON_LEN));
	private static final CharSequence SERVER_NAME = AsciiString.cached("Netty");

	private volatile CharSequence date = new AsciiString(FORMAT.get().format(new Date()));

	HelloServerHandler(ScheduledExecutorService service) {
		service.scheduleWithFixedDelay(new Runnable() {
			private final DateFormat format = FORMAT.get();

			@Override
			public void run() {
				date = new AsciiString(format.format(new Date()));
			}
		}, 1000, 1000, TimeUnit.MILLISECONDS);
	}

	@Override
	public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
		// fast path
		if (msg == LastHttpContent.EMPTY_LAST_CONTENT) {
			return;
		}
		if (msg.getClass() == DefaultHttpRequest.class) {
			DefaultHttpRequest request = (DefaultHttpRequest) msg;
			process(ctx, request);
		} else {
			channelReadSlowPath(ctx, msg);
		}
	}

	private void channelReadSlowPath(ChannelHandlerContext ctx, Object msg) throws Exception {
		// slow path
		if (msg instanceof HttpRequest) {
			try {
				HttpRequest request = (HttpRequest) msg;
				process(ctx, request);
			} finally {
				ReferenceCountUtil.release(msg);
			}
		}
	}

	private void process(ChannelHandlerContext ctx, HttpRequest request) throws Exception {
		String uri = request.uri();
		switch (uri) {
		case "/plaintext":
			writePlainResponse(ctx, Unpooled.wrappedBuffer(STATIC_PLAINTEXT));
			return;
		case "/json":
			byte[] json = serializeMsg(newMsg());
			writeJsonResponse(ctx, Unpooled.wrappedBuffer(json));
			return;
		}
		FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, NOT_FOUND, Unpooled.EMPTY_BUFFER, false);
		ctx.write(response).addListener(ChannelFutureListener.CLOSE);
	}

	private void writePlainResponse(ChannelHandlerContext ctx, ByteBuf buf) {
		ctx.write(makeResponse(buf, TEXT_PLAIN, PLAINTEXT_CLHEADER_VALUE), ctx.voidPromise());
	}

	private void writeJsonResponse(ChannelHandlerContext ctx, ByteBuf buf) {
		ctx.write(makeResponse(buf, APPLICATION_JSON, JSON_CLHEADER_VALUE), ctx.voidPromise());
	}

	private FullHttpResponse makeResponse(ByteBuf buf, CharSequence contentType, CharSequence contentLength) {
		final FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, OK, buf, false);
		response.headers()
				.set(CONTENT_TYPE, contentType)
				.set(SERVER, SERVER_NAME)
				.set(DATE, date)
				.set(CONTENT_LENGTH, contentLength);
		return response;
	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
		ctx.close();
	}

	@Override
	public void channelReadComplete(ChannelHandlerContext ctx) throws Exception {
		ctx.flush();
	}
}
