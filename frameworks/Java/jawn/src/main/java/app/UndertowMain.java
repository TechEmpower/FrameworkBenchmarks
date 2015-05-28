package app;

import net.javapla.jawn.server.UndertowServer;
import net.javapla.jawn.server.spi.ServerConfig;

public class UndertowMain {

    public static void main(String[] args) throws Exception {
        // Automatically set environment to production if nothing is specified
        // Framework defaults to development
        String environment = "production";
        if (args.length > 0) environment = args[0];
        System.setProperty("JAWN_ENV", environment);
        
        ServerConfig config = new ServerConfig();
        config.setContextPath("/");
        config.setPort(8081);
        config.setWebappPath("webapp");
        
        UndertowServer server = new UndertowServer();
        server.setupAndStartServer(config);
    }
}
