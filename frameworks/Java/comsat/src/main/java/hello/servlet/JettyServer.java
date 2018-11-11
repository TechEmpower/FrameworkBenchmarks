package hello.servlet;

import hello.Server;
import co.paralleluniverse.embedded.containers.AbstractEmbeddedServer;

import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;
import org.eclipse.jetty.util.thread.QueuedThreadPool;

/**
 * An implementation of the TechEmpower benchmark tests using Comsat servlets and the Jetty servlet container.
 */
public final class JettyServer implements Server {
    @Override
    public final void run() throws Exception {
        final org.eclipse.jetty.server.Server s = new org.eclipse.jetty.server.Server(new QueuedThreadPool(200, Runtime.getRuntime().availableProcessors()));

        final ServerConnector http = new ServerConnector(s);
        http.setReuseAddress(true);
        http.setAcceptQueueSize(100000);
        http.setPort(8080);
        s.addConnector(http);

        final ServletContextHandler context = new ServletContextHandler();
        context.setContextPath("/");

        final ServletHolder holder1 = new ServletHolder(new PlaintextServlet());
        context.addServlet(holder1, "/plaintext");
        holder1.setAsyncSupported(true);
        final ServletHolder holder2 = new ServletHolder(new JsonServlet());
        context.addServlet(holder2, "/json");
        holder2.setAsyncSupported(true);

        s.setHandler(context);

        s.start();
        System.err.println("Server is up.");

        AbstractEmbeddedServer.waitUrlAvailable("http://localhost:8080/plaintext");
        AbstractEmbeddedServer.waitUrlAvailable("http://localhost:8080/json");
        System.err.println("Server test cases are instrumented and bootstrapped.");

        s.join();
    }
}
