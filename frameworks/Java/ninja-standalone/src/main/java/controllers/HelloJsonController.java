package controllers;

import ninja.Result;
import ninja.Results;

import com.google.inject.Singleton;

@Singleton
public class HelloJsonController {

    public Result index() {
        return Results.json().render(new Message("Hello, World!"));
    }

    public static final class Message {

        public final String message;

        public Message(String message) {
            this.message = message;
        }
    }
}
