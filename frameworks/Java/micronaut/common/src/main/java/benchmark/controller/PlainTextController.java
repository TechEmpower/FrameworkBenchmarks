package benchmark.controller;

import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;

import java.nio.charset.StandardCharsets;

@Controller("/plaintext")
public class PlainTextController {

    private static final byte[] TEXT = "Hello, World!".getBytes(StandardCharsets.UTF_8);

    @Get(value = "/", produces = MediaType.TEXT_PLAIN)
    public byte[] getPlainText() {
        return TEXT;
    }
}
