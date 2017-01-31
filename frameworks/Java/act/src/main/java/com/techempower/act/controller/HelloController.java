package com.techempower.act.controller;

import act.controller.Controller;
import act.handler.NonBlock;
import org.osgl.mvc.annotation.GetAction;
import org.osgl.mvc.annotation.SessionFree;

import javax.inject.Singleton;
import java.io.File;

@Singleton
final class HelloController extends Controller.Util {

    private static final String HELLO_WORLD = "Hello, World!";

    @GetAction("/json")
    @SessionFree
    @NonBlock
    public void json() throws Exception {
        json(new Message(HELLO_WORLD));
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
