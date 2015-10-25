package app.controllers;

import app.models.Message;
import net.javapla.jawn.core.Controller;

public class JsonController extends Controller {

    private static final String message = "Hello, World!";
    public void index() {
        respond().json(new Message(message));
    }
}
