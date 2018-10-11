package com.lostsys.test.javalin;

import io.javalin.Javalin;

import java.util.HashMap;
import java.util.Map;

public class Bench {

    public static void main(String[] args) {

        Javalin app = Javalin.create().start(8080);
        app.get("/plaintext", ctx -> ctx.result("Hello, World!"));

        app.get("/json", ctx -> {
            Map<String, String> hello = new HashMap<>();
            hello.put("message", "Hello, World!");
            ctx.json(hello);
        });

    }

    private static Map<String, String> sayHello() {

        Map<String, String> hello = new HashMap<>();
        hello.put("message", "Hello, World!");
        return hello;
    }

}
