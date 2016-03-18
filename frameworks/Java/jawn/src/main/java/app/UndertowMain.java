package app;

import net.javapla.jawn.server.UndertowServer;
import net.javapla.jawn.server.api.ServerConfig;
import net.javapla.jawn.server.api.ServerConfig.PERFORMANCE_MODE;

public class UndertowMain {

    public static void main(String[] args) throws Exception {
        // Automatically set environment to production if nothing is specified
        // Framework defaults to development
        String environment = "production";
        if (args.length > 0) environment = args[0];
        System.setProperty("JAWN_ENV", environment);
        
        ServerConfig config = new ServerConfig();
        
        config.setContextPath("/")
            .setPort(8080)
            .setWebappPath("webapp")
            .setServerPerformance(PERFORMANCE_MODE.HIGHEST)
            ;
        
        UndertowServer server = new UndertowServer();
        server.setupAndStartServer(config);
    }
}
