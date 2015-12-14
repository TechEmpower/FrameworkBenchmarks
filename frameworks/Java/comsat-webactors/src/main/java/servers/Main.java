package servers;

import co.paralleluniverse.embedded.containers.AbstractEmbeddedServer;

import java.io.IOException;

public class Main {
	public static void main(String[] args) throws Exception {
		final Server server = (Server) Class.forName(System.getProperty("serverClass")).newInstance();

		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				try {
					server.stop();
				} catch (Exception ignored) {
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
	}
}
