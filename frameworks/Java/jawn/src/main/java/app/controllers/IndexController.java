package app.controllers;

import net.javapla.jawn.core.ApplicationController;
import app.models.Message;

public class IndexController extends ApplicationController {

    public void getJson() {
        respond().json(new Message("Hello, World!"));
    }
    
    public void getPlaintext() {
        respond().text("Hello, World!");
    }
}
