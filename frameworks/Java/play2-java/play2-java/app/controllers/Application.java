package controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.With;
import play.libs.Json;
import utils.Headers;

@With(Headers.class)
public class Application extends Controller {

    public static class Message {
        public final String message = "Hello, World!";
    }

    public Result json() {
        return ok(Json.toJson(new Message()));
    }

    public Result plainText() {
        return ok("Hello, World!");
    }

}
