package com.techempower.spring.web;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
final class HelloController {

    private static final String HELLO_WORLD = "Hello, World!";

	@RequestMapping(value = "/json", produces = "application/json")
    Message json() {
        return new Message("Hello, world");
    }

    @RequestMapping(value = "/plaintext", produces = "text/plain")
    String plaintext() {
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
