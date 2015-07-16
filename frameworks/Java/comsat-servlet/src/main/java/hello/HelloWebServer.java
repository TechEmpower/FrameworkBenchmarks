package hello;

import co.paralleluniverse.embedded.containers.AbstractEmbeddedServer;
import co.paralleluniverse.embedded.containers.EmbeddedServer;

import java.io.IOException;

/**
 * An implementation of the TechEmpower benchmark tests using Comsat servlets and the Undertow web server.
 */
public final class HelloWebServer {
	public static void main(String[] args) throws Exception {
		final EmbeddedServer server = (EmbeddedServer) Class.forName(System.getProperty("serverClass")).newInstance();
		server.addServlet("plaintext", PlaintextServlet.class, "/plaintext");
		server.addServlet("json", JsonServlet.class, "/json");

		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				try {
					server.stop();

					System.err.println("Server is down.");
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		});

		new Thread() {
			@Override
			public void run() {
				try {
					AbstractEmbeddedServer.waitUrlAvailable("http://localhost:8080/plaintext");
					AbstractEmbeddedServer.waitUrlAvailable("http://localhost:8080/json");

					System.err.println("Server test cases are instrumented and bootstrapped.");
				} catch (InterruptedException | IOException e) {
					throw new RuntimeException(e);
				}
			}
		}.start();

		server.start();

		System.err.println("Server is up.");
	}
}
