package controllers;

import play.libs.Json;
import play.mvc.Controller;
import play.mvc.Http.MimeTypes;
import play.mvc.Result;

public class Application extends Controller {

    public static class Message {
        public final String message = "Hello, World!";
    }

    public Result json() {
        return ok(Json.toJson(new Message()));
    }

    public Result plainText() {
        return ok("Hello, World!").as(MimeTypes.TEXT);
    }

}
