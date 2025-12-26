package hello;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpHeaderValues;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.HttpVersion;
import io.netty.util.AsciiString;

public final class HttpResponses {

    private static final CharSequence SERVER_NAME = AsciiString.cached("Netty");

    private HttpResponses() {
    }

    public static FullHttpResponse plaintext() {
        ByteBuf buf = Unpooled.wrappedBuffer(Constants.STATIC_PLAINTEXT);
        HttpHeaders headers = new io.netty.handler.codec.http.DefaultHttpHeaders();
        headers.set(HttpHeaderNames.SERVER, SERVER_NAME);
        headers.set(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.TEXT_PLAIN);
        headers.setInt(HttpHeaderNames.CONTENT_LENGTH, Constants.STATIC_PLAINTEXT_LEN);
        return new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK, buf, headers, HttpHeaders.EMPTY_HEADERS);
    }

    public static FullHttpResponse json() {
        byte[] body = JsonUtils.serializeMsg(Constants.STATIC_MESSAGE);
        ByteBuf buf = Unpooled.wrappedBuffer(body);
        HttpHeaders headers = new io.netty.handler.codec.http.DefaultHttpHeaders();
        headers.set(HttpHeaderNames.SERVER, SERVER_NAME);
        headers.set(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.APPLICATION_JSON);
        headers.setInt(HttpHeaderNames.CONTENT_LENGTH, body.length);
        return new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK, buf, headers, HttpHeaders.EMPTY_HEADERS);
    }
}
