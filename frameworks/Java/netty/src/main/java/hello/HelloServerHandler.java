package hello;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.util.AsciiString;
import io.netty.util.CharsetUtil;
import io.netty.util.ReferenceCountUtil;
import io.netty.util.concurrent.FastThreadLocal;

import static io.netty.handler.codec.http.HttpHeaderNames.CONTENT_LENGTH;
import static io.netty.handler.codec.http.HttpHeaderNames.CONTENT_TYPE;
import static io.netty.handler.codec.http.HttpHeaderNames.DATE;
import static io.netty.handler.codec.http.HttpHeaderNames.SERVER;
import static io.netty.handler.codec.http.HttpHeaderValues.APPLICATION_JSON;
import static io.netty.handler.codec.http.HttpHeaderValues.TEXT_PLAIN;
import static io.netty.handler.codec.http.HttpResponseStatus.*;
import static io.netty.handler.codec.http.HttpVersion.*;

public class HelloServerHandler extends ChannelInboundHandlerAdapter {

	private static final FastThreadLocal<DateFormat> FORMAT = new FastThreadLocal<DateFormat>() {
		@Override
		protected DateFormat initialValue() {
			return new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z");
		}
	};

	private static ObjectMapper newMapper() {
		ObjectMapper m = new ObjectMapper();
		m.registerModule(new AfterburnerModule());
		return m;
	}

	private static Message newMsg() {
		return new Message("Hello, World!");
	}

	private static int jsonLen() {
		try {
			return newMapper().writeValueAsBytes(newMsg()).length;
		} catch (JsonProcessingException e) {
			throw new RuntimeException(e);
		}
	}

	private static final byte[] STATIC_PLAINTEXT = "Hello, World!".getBytes(CharsetUtil.UTF_8);
	private static final int STATIC_PLAINTEXT_LEN = STATIC_PLAINTEXT.length;

	private static final ByteBuf PLAINTEXT_CONTENT_BUFFER = Unpooled.unreleasableBuffer(Unpooled.directBuffer().writeBytes(STATIC_PLAINTEXT));
	private static final CharSequence PLAINTEXT_CLHEADER_VALUE = AsciiString.cached(String.valueOf(STATIC_PLAINTEXT_LEN));
	private static final CharSequence JSON_CLHEADER_VALUE = AsciiString.cached(String.valueOf(jsonLen()));
	private static final CharSequence SERVER_NAME = AsciiString.cached("Netty");

	private static final ObjectMapper MAPPER = newMapper();

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
		if (msg instanceof HttpRequest) {
			try {
				HttpRequest request = (HttpRequest) msg;
				process(ctx, request);
			} finally {
				ReferenceCountUtil.release(msg);
			}
		} else {
			ctx.fireChannelRead(msg);
		}
	}

	private void process(ChannelHandlerContext ctx, HttpRequest request) throws Exception {
		String uri = request.uri();
		switch (uri) {
			case "/plaintext":
				writePlainResponse(ctx, PLAINTEXT_CONTENT_BUFFER.duplicate());
				return;
			case "/json":
				byte[] json = MAPPER.writeValueAsBytes(newMsg());
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
