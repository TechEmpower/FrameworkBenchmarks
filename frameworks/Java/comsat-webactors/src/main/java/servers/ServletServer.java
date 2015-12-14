package servers;

import co.paralleluniverse.comsat.webactors.servlet.WebActorInitializer;
import co.paralleluniverse.embedded.containers.AbstractEmbeddedServer;
import co.paralleluniverse.embedded.containers.EmbeddedServer;

public class ServletServer implements Server {
	private EmbeddedServer server;

	@Override
	public void start() throws Exception {
		server = (EmbeddedServer) Class.forName(System.getProperty("servletServerClass")).newInstance();
		WebActorInitializer.setUserClassLoader(ClassLoader.getSystemClassLoader());
		server.addServletContextListener(WebActorInitializer.class);
		server.enableWebsockets();
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
