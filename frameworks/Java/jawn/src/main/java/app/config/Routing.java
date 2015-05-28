package app.config;

import net.javapla.jawn.core.spi.ApplicationRoutes;
import net.javapla.jawn.core.spi.Routes;
import app.controllers.DbController;
import app.controllers.IndexController;


public class Routing implements ApplicationRoutes {
    
    @Override
    public void router(Routes routes) {
        routes.GET().route("/json").to(IndexController.class, "json");
        routes.GET().route("/plaintext").to(IndexController.class, "plaintext");
        routes.GET().route("/queries").to(DbController.class, "queries");
        routes.GET().route("/updates").to(DbController.class, "updates");
    }

}
