package org.glassfish.grizzly.bm;

import java.io.IOException;
import org.glassfish.grizzly.Grizzly;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.grizzly.http.server.NetworkListener;
import org.glassfish.grizzly.nio.transport.TCPNIOTransport;

/**
 * HttpServer
 */
public class Server {
    public static final String SERVER_VERSION = "Grizzly/" + Grizzly.getDotedVersion();
    
    public static void main(String[] args) throws IOException {
        final int port = args.length > 0
                ? Integer.parseInt(args[0]) : 8080;
        
        final HttpServer httpServer = new HttpServer();
        final NetworkListener networkListener = new NetworkListener(
                "http-listener", "0.0.0.0", port);
        final TCPNIOTransport transport = networkListener.getTransport();
        
        // force to not initialize worker thread pool
        transport.setWorkerThreadPoolConfig(null);
        
        networkListener.getKeepAlive().setIdleTimeoutInSeconds(-1);
        networkListener.getKeepAlive().setMaxRequestsCount(-1);
        
        httpServer.addListener(networkListener);
        
        httpServer.getServerConfiguration().addHttpHandler(
                new RootHttpHandler(), "/");
//        httpServer.getServerConfiguration().addHttpHandler(
//                new PlainTextHttpHandler(), "/plaintext");
//        httpServer.getServerConfiguration().addHttpHandler(
//                new JsonHttpHandler(), "/json");
        
        try {
            httpServer.start();
            
            System.err.print("Server started. Press ENTER to stop.\n");
            System.in.read();
        } finally {
            httpServer.stop();
        }
    }
}
