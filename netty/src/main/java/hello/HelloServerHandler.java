package hello;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.MessageList;
import io.netty.channel.SimpleChannelInboundHandler;
import io.netty.handler.codec.http.Cookie;
import io.netty.handler.codec.http.CookieDecoder;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.HttpResponse;
import io.netty.handler.codec.http.ServerCookieEncoder;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;

import java.io.*;

import com.fasterxml.jackson.databind.*;
import io.netty.util.CharsetUtil;

import static io.netty.handler.codec.http.HttpHeaders.Names.*;
import static io.netty.handler.codec.http.HttpHeaders.*;
import static io.netty.handler.codec.http.HttpResponseStatus.*;
import static io.netty.handler.codec.http.HttpVersion.*;

public class HelloServerHandler extends SimpleChannelInboundHandler<Object>{

    /** Buffer that stores the response content */
    private static final ObjectMapper mapper = new ObjectMapper();

    private MessageList<Object> out;

    @Override
    public void beginMessageReceived(ChannelHandlerContext ctx) throws Exception {
        out = MessageList.newInstance();
        super.beginMessageReceived(ctx);
    }

    @Override
    public void messageReceived(ChannelHandlerContext ctx, Object msg) throws Exception {
        if (msg instanceof HttpRequest) {
            HttpRequest request = (HttpRequest) msg;

            if (is100ContinueExpected(request)) {
                send100Continue(out);
            }
            ByteBuf buf = ctx.alloc().buffer();
            if("/plaintext".equals(request.getUri())) {
                buf.writeBytes("Hello, World!".getBytes(CharsetUtil.UTF_8));
            } else {
                Map<String, String> data = new HashMap<String, String>();
                data.put("message", "Hello, world");

                try
                {
                  buf.writeBytes(HelloServerHandler.mapper.writeValueAsBytes(data));
                }
                catch (IOException ex)
                {
                  // do nothing
                }
            }
            writeResponse(ctx, request, buf, out);
        }
    }

    private void writeResponse(ChannelHandlerContext ctx, HttpRequest request, ByteBuf buf, MessageList<Object> out) {
        // Decide whether to close the connection or not.
        boolean keepAlive = isKeepAlive(request);
        // Build the response object.
        FullHttpResponse response = new DefaultFullHttpResponse(
                HTTP_1_1, OK, buf);

        response.headers().set(CONTENT_TYPE, "text/plain; charset=UTF-8");

        if (keepAlive) {
            // Add 'Content-Length' header only for a keep-alive connection.
            response.headers().set(CONTENT_LENGTH, response.content().readableBytes());
            // Add keep alive header as per:
            // - http://www.w3.org/Protocols/HTTP/1.1/draft-ietf-http-v11-spec-01.html#Connection
            response.headers().set(CONNECTION, HttpHeaders.Values.KEEP_ALIVE);
        }

        // Encode the cookie.
        String cookieString = request.headers().get(COOKIE);
        if (cookieString != null) {
            Set<Cookie> cookies = CookieDecoder.decode(cookieString);
            if (!cookies.isEmpty()) {
                // Reset the cookies if necessary.
                for (Cookie cookie: cookies) {
                    response.headers().add(SET_COOKIE, ServerCookieEncoder.encode(cookie));
                }
            }
        } else {
            // Browser sent no cookie. Add some.
            response.headers().add(SET_COOKIE, ServerCookieEncoder.encode("key1", "value1"));
            response.headers().add(SET_COOKIE, ServerCookieEncoder.encode("key2", "value2"));
        }

        // Write the response.
        out.add(response);

        // Close the non-keep-alive connection after the write operation is done.
        if (!keepAlive) {
            this.out = MessageList.newInstance();
            ctx.write(out).addListener(ChannelFutureListener.CLOSE);
        }
    }

    private void send100Continue(MessageList<Object> out) {
        HttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, CONTINUE, Unpooled.EMPTY_BUFFER);
        out.add(response);
    }

    @Override
    public void endMessageReceived(ChannelHandlerContext ctx) throws Exception {
        if (out != null) {
            MessageList<Object> msgs = out;
            this.out = null;
            ctx.write(msgs);
        }
    }

    @Override
    public void exceptionCaught(
            ChannelHandlerContext ctx, Throwable cause) throws Exception {
        cause.printStackTrace();
        ctx.close();
    }
}
