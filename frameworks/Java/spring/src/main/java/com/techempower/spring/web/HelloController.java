package com.techempower.spring.web;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
final class HelloController {

    private static final String HELLO_WORLD = "Hello, World!";
    private static final Message MESSAGE = new Message(HELLO_WORLD);

    @RequestMapping(value = "/json", produces = "application/json")
    public Message json() {
        return MESSAGE;
    }

    @RequestMapping(value = "/plaintext", produces = "text/plain")
    public String plaintext() {
        return HELLO_WORLD;
    }

    public static final class Message {

        private final String message;

        private Message(String message) {
            this.message = message;
        }

        public String getMessage() {
            return message;
        }

    }

}
