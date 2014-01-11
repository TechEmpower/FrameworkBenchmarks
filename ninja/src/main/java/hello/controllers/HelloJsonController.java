package hello.controllers;

import ninja.Result;
import ninja.Results;

import com.google.inject.Singleton;

@Singleton
public class HelloJsonController {

    public Result index() {
	return Results.json().render(new Message("Hello, World!"));
    }

    public static class Message {

	private final String message;

	public Message(String message) {
	    this.message = message;
	}

	public String getMessage() {
	    return message;
	}
    }
}
