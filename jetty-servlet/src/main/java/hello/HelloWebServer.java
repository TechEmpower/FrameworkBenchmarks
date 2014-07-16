package hello;

import org.eclipse.jetty.server.HttpConfiguration;
import org.eclipse.jetty.server.HttpConnectionFactory;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.servlet.ServletContextHandler;


/**
 * An implementation of the TechEmpower benchmark tests using the Jetty web
 * server.  
 */
public final class HelloWebServer 
{

    public static void main(String[] args) throws Exception
    {
        Server server = new Server(8080);
        ServerConnector connector = server.getBean(ServerConnector.class);
        HttpConfiguration config = connector.getBean(HttpConnectionFactory.class).getHttpConfiguration();
        config.setSendDateHeader(true);
        config.setSendServerVersion(true);

        ServletContextHandler context = new ServletContextHandler(ServletContextHandler.NO_SECURITY|ServletContextHandler.NO_SESSIONS);
        context.setContextPath("/");
        server.setHandler(context);

        context.addServlet(org.eclipse.jetty.servlet.DefaultServlet.class,"/");
        context.addServlet(JsonServlet.class,"/json");
        context.addServlet(PlaintextServlet.class,"/plaintext");

        server.start();
        server.join();
    }
}
