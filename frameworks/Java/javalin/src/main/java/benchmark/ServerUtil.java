package benchmark;

import io.javalin.util.ConcurrencyUtil;
import org.eclipse.jetty.http.UriCompliance;
import org.eclipse.jetty.server.HttpConfiguration;
import org.eclipse.jetty.server.HttpConnectionFactory;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;

public class ServerUtil {
    /**
     * This is a workaround for the {@code Server} header required by the TechEmpower Framework Benchmarks.
     * Simple server with a single HTTP/1.1 connector.
     *
     * @return a new Javalin server with the {@code Server} header enabled.
     */
    public static Server createServer() {
        Server server = new Server(ConcurrencyUtil.jettyThreadPool("JettyServerThreadPool", 8, 250));
        ServerConnector connector;

        //The http configuration object
        HttpConfiguration httpConfiguration = new HttpConfiguration();
        httpConfiguration.setUriCompliance(UriCompliance.RFC3986);  // accept ambiguous values in path and let Javalin handle them

        //The factory for HTTP/1.1 connections.
        HttpConnectionFactory http11 = new HttpConnectionFactory(httpConfiguration);

        //The factory for HTTP/2 connections.
        connector = new ServerConnector(server, http11);

        connector.setPort(8080);

        server.addConnector(connector);

        return server;
    }
}
