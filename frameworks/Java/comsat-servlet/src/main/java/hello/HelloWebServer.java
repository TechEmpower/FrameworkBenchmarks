package hello;

import co.paralleluniverse.embedded.containers.EmbeddedServer;

/**
 * An implementation of the TechEmpower benchmark tests using Comsat servlets and the Undertow web server.
 */
public final class HelloWebServer {
	public static void main(String[] args) throws Exception {
		final EmbeddedServer server = (EmbeddedServer) Class.forName(System.getProperty("serverClass")).newInstance();
		server.addServlet("plaintext", PlaintextServlet.class, "/plaintext");
		server.addServlet("json", JsonServlet.class, "/json");
		server.start();
		System.err.println("Server is up");
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				try {
					server.stop();
					System.err.println("Server is down");
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		});
	}
}
