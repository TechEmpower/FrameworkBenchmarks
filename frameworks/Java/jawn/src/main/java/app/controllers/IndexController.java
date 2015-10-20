package app.controllers;

import net.javapla.jawn.core.Controller;
import app.models.Message;

public class IndexController extends Controller {

    public void getJson() {
        respond().json(new Message("Hello, World!"));
    }
    
    public void getPlaintext() {
        respond().text("Hello, World!");
    }
}
