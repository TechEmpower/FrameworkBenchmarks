package com.techempower.spring.web;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
final class HelloController {

    @RequestMapping(value = "/json", produces = "application/json")
    Message json() {
        return new Message("Hello, world");
    }

    @RequestMapping(value = "/plaintext", produces = "text/plain")
    String plaintext() {
        return "Hello, World!";
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
