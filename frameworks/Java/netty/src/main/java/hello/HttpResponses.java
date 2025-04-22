package hello;

import static hello.Constants.JSON_CLHEADER_VALUE;
import static hello.Constants.PLAINTEXT_CLHEADER_VALUE;
import static hello.Constants.SERVER_NAME;
import static hello.Constants.STATIC_PLAINTEXT;
import static hello.Constants.newMsg;
import static hello.JsonUtils.serializeMsg;
import static io.netty.handler.codec.http.HttpHeaderNames.CONTENT_LENGTH;
import static io.netty.handler.codec.http.HttpHeaderNames.CONTENT_TYPE;
import static io.netty.handler.codec.http.HttpHeaderNames.DATE;
import static io.netty.handler.codec.http.HttpHeaderNames.SERVER;
import static io.netty.handler.codec.http.HttpHeaderValues.APPLICATION_JSON;
import static io.netty.handler.codec.http.HttpHeaderValues.TEXT_PLAIN;
import static io.netty.handler.codec.http.HttpResponseStatus.OK;
import static io.netty.handler.codec.http.HttpVersion.HTTP_1_1;

import com.jsoniter.output.JsonStream;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.DefaultHttpHeadersFactory;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpHeadersFactory;
import io.netty.util.AsciiString;

public class HttpResponses {

   private static final HttpHeadersFactory HEADERS_FACTORY = DefaultHttpHeadersFactory.headersFactory()
         .withValidation(false);

   public static HttpHeaders makeJsonHeaders(AsciiString date) {
      return HEADERS_FACTORY.newHeaders()
            .set(CONTENT_TYPE, APPLICATION_JSON)
            .set(SERVER, SERVER_NAME)
            .set(DATE, date)
            .set(CONTENT_LENGTH, JSON_CLHEADER_VALUE);
   }


   public static HttpHeaders makePlaintextHeaders(AsciiString date) {
      return HEADERS_FACTORY.newHeaders()
            .set(CONTENT_TYPE, TEXT_PLAIN)
            .set(SERVER, SERVER_NAME)
            .set(DATE, date)
            .set(CONTENT_LENGTH, PLAINTEXT_CLHEADER_VALUE);
   }

   public static FullHttpResponse makePlaintextResponse(HttpHeaders plaintextHeaders) {
      return makeResponse(Unpooled.wrappedBuffer(STATIC_PLAINTEXT), plaintextHeaders);
   }

   public static FullHttpResponse makeJsonResponse(JsonStream stream, HttpHeaders jsonHeaders) {
      return makeResponse(Unpooled.wrappedBuffer(serializeMsg(newMsg(), stream)), jsonHeaders);
   }

   private static FullHttpResponse makeResponse(ByteBuf buf, HttpHeaders headers) {
      return new DefaultFullHttpResponse(HTTP_1_1, OK, buf, headers, HttpHeaders.EMPTY_HEADERS);
   }
}
