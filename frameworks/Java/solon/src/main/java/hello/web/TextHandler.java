package hello.web;

import org.noear.solon.annotation.Component;
import org.noear.solon.boot.web.MimeType;
import org.noear.solon.core.handle.Context;
import org.noear.solon.core.handle.Handler;

import java.nio.charset.StandardCharsets;

@Component
public class TextHandler implements Handler {
    private static final byte[] TEXT_BODY = "Hello, World!".getBytes(StandardCharsets.UTF_8);

    private static final String TEXT_BODY_LENGTH = String.valueOf(TEXT_BODY.length);
    private static final String CONTENT_LENGTH = "Content-Length";
    private static final String CONTENT_TYPE = "Content-Type";

    @Override
    public void handle(Context ctx) throws Throwable {
        ctx.headerSet(CONTENT_LENGTH, TEXT_BODY_LENGTH);
        ctx.headerSet(CONTENT_TYPE, MimeType.TEXT_PLAIN_VALUE);
        ctx.output("Hello, World!".getBytes());
    }
}

