package app.config;

import net.javapla.jawn.core.ApplicationConfig;
import net.javapla.jawn.core.api.ApplicationBootstrap;
import app.db.DbModule;

public class Bootstrap implements ApplicationBootstrap {

    @Override
    public void bootstrap(ApplicationConfig config) {
        config.registerModules(new DbModule());
    }

    @Override
    public void destroy() {}

}
