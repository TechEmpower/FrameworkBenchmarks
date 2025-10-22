package hello;

import static hello.HttpResponses.*;
import static hello.JsonUtils.acquireJsonStreamFromEventLoop;
import static hello.JsonUtils.releaseJsonStreamFromEventLoop;
import static io.netty.handler.codec.http.HttpResponseStatus.NOT_FOUND;
import static io.netty.handler.codec.http.HttpVersion.HTTP_1_1;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import com.jsoniter.output.JsonStream;

import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.handler.codec.http.*;
import io.netty.util.AsciiString;
import io.netty.util.ReferenceCountUtil;
import io.netty.util.concurrent.FastThreadLocal;

public class HelloServerHandler extends ChannelInboundHandlerAdapter {

	private static final FastThreadLocal<DateFormat> FORMAT = new FastThreadLocal<>() {
      @Override
      protected DateFormat initialValue() {
         return new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z");
      }
   };

	private HttpHeaders jsonHeaders = makeJsonHeaders(new AsciiString(FORMAT.get().format(new Date())));
	private HttpHeaders plaintextHeaders = makePlaintextHeaders(new AsciiString(FORMAT.get().format(new Date())));
	private ScheduledFuture<?> refreshHeaders;

	public HelloServerHandler(ScheduledExecutorService service) {
		refreshHeaders = service.scheduleWithFixedDelay(new Runnable() {
			private final DateFormat format = FORMAT.get();

			@Override
			public void run() {
				var date = new AsciiString(format.format(new Date()));
				jsonHeaders = makeJsonHeaders(date);
				plaintextHeaders = makePlaintextHeaders(date);
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
			writePlainResponse(ctx, plaintextHeaders);
			return;
		case "/json":
			// even for the virtual thread case we expect virtual threads to be executed inlined!
			var stream = acquireJsonStreamFromEventLoop();
			try {
				writeJsonResponse(ctx, stream, jsonHeaders);
			} finally {
				releaseJsonStreamFromEventLoop(stream);
			}
			return;
		}
		// we drain in-flight responses before closing the connection
		channelReadComplete(ctx);
		FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, NOT_FOUND, Unpooled.EMPTY_BUFFER, false);
		ctx.writeAndFlush(response).addListener(ChannelFutureListener.CLOSE);
	}

	protected void writePlainResponse(ChannelHandlerContext ctx, HttpHeaders plaintextHeaders) {
		ctx.write(makePlaintextResponse(plaintextHeaders), ctx.voidPromise());
	}

	protected void writeJsonResponse(ChannelHandlerContext ctx, JsonStream stream, HttpHeaders jsonHeaders) {
		ctx.write(makeJsonResponse(stream, jsonHeaders), ctx.voidPromise());
	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
		ctx.close();
	}

	@Override
	public void channelReadComplete(ChannelHandlerContext ctx) {
		ctx.flush();
	}

	@Override
	public void channelInactive(ChannelHandlerContext ctx) throws Exception {
		super.channelInactive(ctx);
		if (refreshHeaders != null) {
			refreshHeaders.cancel(false);
			refreshHeaders = null;
		}
	}
}
