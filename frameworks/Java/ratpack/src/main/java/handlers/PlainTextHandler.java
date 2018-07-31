package handlers;

import ratpack.handling.Context;
import ratpack.handling.Handler;

public class PlainTextHandler implements Handler {
    private static final String MESSAGE = "Hello, World!";

    @Override
    public void handle(Context ctx) {
        ctx.getResponse().send(MESSAGE);
    }
}
