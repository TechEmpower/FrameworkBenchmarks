package hello;

import co.paralleluniverse.embedded.containers.EmbeddedServer;
import co.paralleluniverse.embedded.containers.UndertowServer;

/**
 * An implementation of the TechEmpower benchmark tests using Comsat servlets and the Undertow web server.
 */
public final class HelloWebServer  {
    public static void main(String[] args) throws Exception {
        final EmbeddedServer server = new UndertowServer();
        server.addServlet("plaintext", PlaintextServlet.class, "/plaintext");
        server.addServlet("json", JsonServlet.class, "/json");
        server.start();
    }
}
