package hello.handler;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

import org.eclipse.jetty.http.HttpField;
import org.eclipse.jetty.http.HttpHeader;
import org.eclipse.jetty.http.MimeTypes;
import org.eclipse.jetty.http.PreEncodedHttpField;
import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.Response;
import org.eclipse.jetty.util.Callback;

public class PlainTextHandler extends Handler.Abstract {
    private static final String RESPONSE_BODY = "Hello, World!";
    private static final HttpField contentType = new PreEncodedHttpField(HttpHeader.CONTENT_TYPE, MimeTypes.Type.TEXT_PLAIN.asString());

    @Override
    public boolean handle(Request request, Response response, Callback callback) {
        try {
            response.setStatus(200);
            response.getHeaders().put(contentType);

            byte[] contentBytes = RESPONSE_BODY.getBytes(StandardCharsets.UTF_8);
            ByteBuffer contentBuffer = ByteBuffer.wrap(contentBytes);
            response.getHeaders().put("Content-Length", String.valueOf(contentBytes.length));

            response.write(true, contentBuffer, callback);
        } catch (Exception e) {
            callback.failed(e);
        }
        return true;
    }
}
