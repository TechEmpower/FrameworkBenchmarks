package servers;

import co.paralleluniverse.comsat.webactors.undertow.AutoWebActorHandler;
import io.undertow.Undertow;

public class UndertowServer implements Server {
	private Undertow server;

	@Override
	public void start() throws Exception {
		server = Undertow.builder()
			.addHttpListener(8080, "localhost")
			.setHandler(new AutoWebActorHandler()).build();
		server.start();

		System.err.println("Server is up.");
	}

	@Override
	public void stop() throws Exception {
		if (server != null) {
			server.stop();

			System.err.println("Server is down.");
		}
	}
}
