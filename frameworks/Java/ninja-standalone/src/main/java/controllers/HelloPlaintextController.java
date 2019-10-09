package controllers;

import ninja.Result;
import ninja.Results;

import com.google.inject.Singleton;

@Singleton
public class HelloPlaintextController {

    public Result index() {
    	//Cache control header is set to disable the double setting of the date header.
        return Results.text().render("Hello, World!").addHeader(Result.CACHE_CONTROL, "");
    }

}
