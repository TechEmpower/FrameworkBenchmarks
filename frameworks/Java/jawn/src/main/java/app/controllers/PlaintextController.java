package app.controllers;

import net.javapla.jawn.core.Controller;

public class PlaintextController extends Controller {

    private static final String message = "Hello, World!";
    private static final byte[] bytemessage = message.getBytes();
    public void index() {
        respond().text(bytemessage);
    }
}
