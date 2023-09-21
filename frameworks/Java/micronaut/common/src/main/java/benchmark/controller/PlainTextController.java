package benchmark.controller;

import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;

import java.nio.charset.StandardCharsets;

@Controller("/plaintext")
public class PlainTextController {

    private static final ByteBuf TEXT = Unpooled.copiedBuffer("Hello, World!".getBytes(StandardCharsets.UTF_8));

    @Get(value = "/", produces = MediaType.TEXT_PLAIN)
    public ByteBuf getPlainText() {
        return TEXT;
    }
}
