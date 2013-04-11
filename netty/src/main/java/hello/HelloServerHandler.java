package hello;

import io.netty.buffer.MessageBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundMessageHandlerAdapter;
import io.netty.handler.codec.DecoderResult;
import io.netty.handler.codec.http.Cookie;
import io.netty.handler.codec.http.CookieDecoder;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.DefaultHttpResponse;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpContent;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpObject;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.HttpResponse;
import io.netty.handler.codec.http.LastHttpContent;
import io.netty.handler.codec.http.ServerCookieEncoder;
import io.netty.util.CharsetUtil;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;

import java.io.*;

import com.fasterxml.jackson.databind.*;

import static io.netty.handler.codec.http.HttpHeaders.Names.*;
import static io.netty.handler.codec.http.HttpHeaders.*;
import static io.netty.handler.codec.http.HttpResponseStatus.*;
import static io.netty.handler.codec.http.HttpVersion.*;

public class HelloServerHandler extends ChannelInboundMessageHandlerAdapter<Object> {

    private HttpRequest request;
    /** Buffer that stores the response content */
    private final StringBuilder buf = new StringBuilder();
    private static final ObjectMapper mapper = new ObjectMapper();

    private boolean flush;

    @Override
    public boolean beginMessageReceived(ChannelHandlerContext ctx) throws Exception {
        flush = false;
        return super.beginMessageReceived(ctx);
    }

    @Override
    public void messageReceived(ChannelHandlerContext ctx, Object msg) throws Exception {
        MessageBuf<Object> out = ctx.nextOutboundMessageBuffer();
        if (msg instanceof HttpRequest) {
            HttpRequest request = this.request = (HttpRequest) msg;

            if (is100ContinueExpected(request)) {
                send100Continue(out);
            }
            
            Map<String, String> data = new HashMap<String, String>();
            data.put("message", "Hello, world");
            
            buf.setLength(0);
            try
            {
              buf.append(HelloServerHandler.mapper.writeValueAsString(data));
            }
            catch (IOException ex)
            {
              // do nothing
            }

            appendDecoderResult(buf, request);
        }

        if (msg instanceof HttpContent) {
            if (msg instanceof LastHttpContent) {
              LastHttpContent trailer = (LastHttpContent) msg;
              writeResponse(ctx, out, trailer);
            }
        }
    }

    private static void appendDecoderResult(StringBuilder buf, HttpObject o) {
        DecoderResult result = o.getDecoderResult();
        if (result.isSuccess()) {
            return;
        }

        buf.append(".. WITH DECODER FAILURE: ");
        buf.append(result.cause());
        buf.append("\r\n");
    }

    private void writeResponse(ChannelHandlerContext ctx, MessageBuf<Object> out, HttpObject currentObj) {
        // Decide whether to close the connection or not.
        boolean keepAlive = isKeepAlive(request);
        // Build the response object.
        FullHttpResponse response = new DefaultFullHttpResponse(
                HTTP_1_1, currentObj.getDecoderResult().isSuccess()? OK : BAD_REQUEST,
                Unpooled.copiedBuffer(buf.toString(), CharsetUtil.UTF_8));

        response.headers().set(CONTENT_TYPE, "text/plain; charset=UTF-8");

        if (keepAlive) {
            // Add 'Content-Length' header only for a keep-alive connection.
            response.headers().set(CONTENT_LENGTH, response.data().readableBytes());
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
            flush = false;
            ctx.flush().addListener(ChannelFutureListener.CLOSE);
        } else {
            flush = true;
        }
    }

    private void send100Continue(MessageBuf<Object> out) {
        HttpResponse response = new DefaultHttpResponse(HTTP_1_1, CONTINUE);
        out.add(response);
        flush = true;
    }

    @Override
    public void endMessageReceived(ChannelHandlerContext ctx) throws Exception {
        if (flush) {
            ctx.flush();
        }
    }

    @Override
    public void exceptionCaught(
            ChannelHandlerContext ctx, Throwable cause) throws Exception {
        cause.printStackTrace();
        ctx.close();
    }
}