package controllers;

import ninja.Result;
import ninja.Results;

import com.google.inject.Singleton;

@Singleton
public class HelloJsonController {

    public Result index() {
    	//Cache control header is set to disable the double setting of the date header.
        return Results.json().render(new Message("Hello, World!")).addHeader(Result.CACHE_CONTROL, "");
    }

    public static final class Message {

        public final String message;

        public Message(String message) {
            this.message = message;
        }
    }
}
