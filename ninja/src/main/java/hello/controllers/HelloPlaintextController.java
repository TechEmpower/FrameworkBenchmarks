package hello.controllers;

import ninja.Result;
import ninja.Results;

import com.google.inject.Singleton;

@Singleton
public class HelloPlaintextController {
    public Result index() {
	return Results.html().renderRaw("Hello, world!");
    }
}
