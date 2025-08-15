package hello.handler;

import org.eclipse.jetty.http.HttpField;
import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.Response;
import org.eclipse.jetty.util.Callback;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;


public class JsonHandler extends Handler.Abstract {

    private static final HttpField JSON_HEADER = new HttpField("Content-Type", "application/json");
    private static final String JSON_RESPONSE = "{\"message\": \"Hello, World!\"}";


    @Override
    public boolean handle(Request request, Response response, Callback callback) {
        try {
            response.setStatus(200);
            response.getHeaders().add(JSON_HEADER);

            byte[] contentBytes = JSON_RESPONSE.getBytes(StandardCharsets.UTF_8);
            ByteBuffer contentBuffer = ByteBuffer.wrap(contentBytes);
            response.getHeaders().put("Content-Length", String.valueOf(contentBytes.length));

            response.write(true, contentBuffer, callback);
        } catch (Exception e) {
            callback.failed(e);
        }
        return true;
    }
}