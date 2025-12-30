package hello.web;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import org.noear.solon.annotation.Component;
import org.noear.solon.boot.web.MimeType;
import org.noear.solon.core.handle.Context;
import org.noear.solon.core.handle.Handler;

import java.util.Map;

@Component
public class JsonHandler implements Handler {
    private final ObjectWriter writer;

    public JsonHandler() {
        this.writer = new ObjectMapper().writerFor(Map.class);
    }

    @Override
    public void handle(Context ctx) throws Throwable {
        byte[] body = this.writer.writeValueAsBytes(Map.of("message", "Hello, world!"));
        ctx.contentLength(body.length);
        ctx.contentType(MimeType.APPLICATION_JSON_VALUE);
        ctx.output(body);
    }
}
