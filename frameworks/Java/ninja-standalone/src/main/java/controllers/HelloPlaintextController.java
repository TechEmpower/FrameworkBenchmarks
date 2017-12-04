package controllers;

import ninja.Result;
import ninja.Results;

import com.google.inject.Singleton;

@Singleton
public class HelloPlaintextController {

    public Result index() {
        return Results.text().render("Hello, World!");
    }

}
