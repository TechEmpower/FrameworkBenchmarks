package app.config;

import net.javapla.jawn.core.Responses;
import net.javapla.jawn.core.api.ApplicationRoutes;
import net.javapla.jawn.core.api.Routes;
import app.controllers.DbController;
import app.models.Message;


public class Routing implements ApplicationRoutes {
    
    private static final String message = "Hello, World!";
    private static final byte[] bytemessage = message.getBytes();
    
    @Override
    public void router(Routes routes) {
        routes.GET().route("/queries").to(DbController.class, "queries");
        routes.GET().route("/updates").to(DbController.class, "updates");
        
        routes.GET().route("/json").with(Responses.json(new Message(message)));
        routes.GET().route("/plaintext").with(Responses.text(bytemessage));
    }

}
