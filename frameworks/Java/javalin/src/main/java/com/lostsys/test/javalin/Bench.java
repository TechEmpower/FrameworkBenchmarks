package com.lostsys.test.javalin;

import io.javalin.Javalin;

import java.util.Collections;

public class Bench {

    public static void main(String[] args) {

        Javalin app = Javalin.create().start(8080);
        app.get("/plaintext", ctx -> ctx.result("Hello, World!"));

        app.get("/json", ctx -> {
            ctx.json(Collections.singletonMap("message", "Hello, World!"));
        });

    }
}
