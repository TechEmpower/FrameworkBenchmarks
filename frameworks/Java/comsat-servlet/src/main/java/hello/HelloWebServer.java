package hello;

import co.paralleluniverse.embedded.containers.EmbeddedServer;

/**
 * An implementation of the TechEmpower benchmark tests using Comsat servlets and the Undertow web server.
 */
public final class HelloWebServer  {
    public static void main(String[] args) throws Exception {
        final EmbeddedServer server = (EmbeddedServer) Class.forName(System.getProperty("serverClass")).newInstance();
        server.addServlet("plaintext", PlaintextServlet.class, "/plaintext");
        server.addServlet("json", JsonServlet.class, "/json");
        server.start();
    }
}
