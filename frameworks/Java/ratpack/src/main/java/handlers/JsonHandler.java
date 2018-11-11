package handlers;

import ratpack.handling.Context;
import ratpack.handling.Handler;

import java.util.Collections;

import static ratpack.jackson.Jackson.json;

public class JsonHandler implements Handler {
    private static final String MESSAGE = "message";
    private static final String HELLO = "Hello, World!";

    @Override
    public void handle(Context ctx) {
        ctx.render(json(Collections.singletonMap(MESSAGE, HELLO)));
    }
}
