package hello;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;

import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import com.fasterxml.jackson.databind.*;
import io.netty.channel.Channel;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.SimpleChannelInboundHandler;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.HttpVersion;
import io.netty.util.CharsetUtil;


public class HelloServerHandler extends SimpleChannelInboundHandler<Object> {
    private final SimpleDateFormat format = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z");
    private CharSequence date;

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private final ByteBuf buffer = Unpooled.directBuffer().writeBytes("Hello, World!".getBytes(CharsetUtil.UTF_8));
    private final CharSequence contentLength = HttpHeaders.newEntity(String.valueOf(buffer.readableBytes()));

    private static final CharSequence TYPE_PLAIN = HttpHeaders.newEntity("text/plain; charset=UTF-8");
    private static final CharSequence TYPE_JSON = HttpHeaders.newEntity("application/json; charset=UTF-8");

    private static final CharSequence SERVER_NAME = HttpHeaders.newEntity("Netty");
    private static final CharSequence CONTENT_TYPE_ENTITY = HttpHeaders.newEntity(HttpHeaders.Names.CONTENT_TYPE);
    private static final CharSequence DATE_ENTITY = HttpHeaders.newEntity(HttpHeaders.Names.DATE);
    private static final CharSequence CONTENT_LENGTH_ENTITY = HttpHeaders.newEntity(HttpHeaders.Names.CONTENT_LENGTH);
    private static final CharSequence SERVER_ENTITY = HttpHeaders.newEntity(HttpHeaders.Names.SERVER);

    @Override
    public void channelRead0(ChannelHandlerContext ctx, Object msg) throws Exception {
        if (msg instanceof HttpRequest) {
            HttpRequest request = (HttpRequest) msg;
            String uri = request.getUri();
            switch (uri) {
                case "/plaintext":
                    writeResponse(ctx, request, buffer.duplicate().retain(), TYPE_PLAIN, contentLength);
                    return;
                case "/json":
                    byte[] json = MAPPER.writeValueAsBytes(Collections.singletonMap("message", "Hello, World!"));
                    writeResponse(ctx, request, ctx.alloc().buffer(json.length).writeBytes(json), TYPE_JSON,
                            String.valueOf(json.length));
                    return;
            }
            FullHttpResponse response = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND, Unpooled.EMPTY_BUFFER, false);
            ctx.write(response).addListener(ChannelFutureListener.CLOSE);
        }
    }

    private void writeResponse(ChannelHandlerContext ctx, HttpRequest request, ByteBuf buf,
                               CharSequence contentType, CharSequence contentLength) {
        // Decide whether to close the connection or not.
        boolean keepAlive = HttpHeaders.isKeepAlive(request);
        // Build the response object.
        FullHttpResponse response = new DefaultFullHttpResponse(
                HttpVersion.HTTP_1_1, HttpResponseStatus.OK, buf, false);
        HttpHeaders headers = response.headers();
        headers.set(CONTENT_TYPE_ENTITY, contentType);
        headers.set(SERVER_ENTITY, SERVER_NAME);
        headers.set(DATE_ENTITY, date);

        if (keepAlive) {
            headers.set(CONTENT_LENGTH_ENTITY, contentLength);
        }

        // Close the non-keep-alive connection after the write operation is done.
        if (!keepAlive) {
            ctx.write(response).addListener(ChannelFutureListener.CLOSE);
        } else {
            ctx.write(response, ctx.voidPromise());
        }
    }

    @Override
    public void exceptionCaught(
            ChannelHandlerContext ctx, Throwable cause) throws Exception {
        ctx.close();
    }

    @Override
    public void channelReadComplete(ChannelHandlerContext ctx) throws Exception {
        ctx.flush();
    }

    @Override
    public void handlerRemoved(ChannelHandlerContext ctx) throws Exception {
        super.handlerRemoved(ctx);
        buffer.release();
    }

    @Override
    public void channelActive(ChannelHandlerContext ctx) throws Exception {
        date = HttpHeaders.newEntity(format.format(new Date()));

        Channel channel = ctx.channel();
        final ScheduledFuture<?> future = channel.eventLoop().scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                date = HttpHeaders.newEntity(format.format(new Date()));
            }
        }, 1000, 1000, TimeUnit.MILLISECONDS);

        channel.closeFuture().addListener(new ChannelFutureListener() {
            @Override
            public void operationComplete(ChannelFuture channelFuture) throws Exception {
                future.cancel(false);
            }
        });
    }
}
