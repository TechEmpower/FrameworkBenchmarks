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
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.util.AsciiString;

public class HttpResponses {

   public static FullHttpResponse makePlaintextResponse(AsciiString date) {
      return makeResponse(Unpooled.wrappedBuffer(STATIC_PLAINTEXT), TEXT_PLAIN, PLAINTEXT_CLHEADER_VALUE, date);
   }

   public static FullHttpResponse makeJsonResponse(JsonStream stream, AsciiString date) {
      return makeResponse(Unpooled.wrappedBuffer(serializeMsg(newMsg(), stream)), APPLICATION_JSON, JSON_CLHEADER_VALUE, date);
   }

   private static FullHttpResponse makeResponse(ByteBuf buf, CharSequence contentType, CharSequence contentLength, AsciiString date) {
      final FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, OK, buf, false);
      response.headers()
            .set(CONTENT_TYPE, contentType)
            .set(SERVER, SERVER_NAME)
            .set(DATE, date)
            .set(CONTENT_LENGTH, contentLength);
      return response;
   }
}
