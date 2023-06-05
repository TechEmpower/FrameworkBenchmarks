package benchmark.controller;

import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;

@Controller("/plaintext")
public class PlainTextController {

    private static final String TEXT = "Hello, World!";

    @Get(value = "/", produces = MediaType.TEXT_PLAIN)
    public String getPlainText() {
        return TEXT;
    }
}
