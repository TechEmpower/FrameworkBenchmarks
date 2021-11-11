package hello;

import java.io.IOException;
import java.net.URI;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.glassfish.grizzly.filterchain.FilterChainBuilder;
import org.glassfish.grizzly.http.server.AddOn;
import org.glassfish.grizzly.http.server.FileCacheFilter;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.grizzly.http.server.NetworkListener;
import org.glassfish.grizzly.http.server.util.HttpPipelineOptAddOn;
import org.glassfish.grizzly.memory.PooledMemoryManager;
import org.glassfish.grizzly.nio.transport.TCPNIOTransport;
import org.glassfish.grizzly.utils.IdleTimeoutFilter;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.server.ResourceConfig;

public class WebServer {
	private static final URI BASE_URI = URI.create("http://0.0.0.0:8080/");

	public static void main(String[] args) {
		try {
			final HttpServer server = GrizzlyHttpServerFactory.createHttpServer(BASE_URI,
					createApp(), false);
			Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
				@Override
				public void run() {
					server.shutdownNow();
				}
			}));

			// Some modifications
			NetworkListener defaultListener = server.getListener("grizzly");
			defaultListener.getKeepAlive().setIdleTimeoutInSeconds(-1);
			defaultListener.getKeepAlive().setMaxRequestsCount(-1);
			defaultListener.getFileCache().setEnabled(false);
			defaultListener.registerAddOn(new SimplifyAddOn());
			defaultListener.registerAddOn(new HttpPipelineOptAddOn());

			final TCPNIOTransport transport = defaultListener.getTransport();
			transport.setWorkerThreadPoolConfig(null); // force to not
														// initialize worker
														// thread pool
			transport.setSelectorRunnersCount(Runtime.getRuntime().availableProcessors() * 2);
			transport.setMemoryManager(new PooledMemoryManager());

			server.start();

			System.out.println(String
					.format("TFBApplication started.%nStop the application using CTRL+C"));

			Thread.currentThread().join();
		} catch (IOException | InterruptedException ex) {
			Logger.getLogger(WebServer.class.getName()).log(Level.SEVERE, null, ex);
		}
	}

	public static ResourceConfig createApp() {
		return new TFBApplication();
	}

	private static class SimplifyAddOn implements AddOn {
		@Override
		public void setup(final NetworkListener networkListener, final FilterChainBuilder builder) {
			final int fcIdx = builder.indexOfType(FileCacheFilter.class);
			if (fcIdx != -1) {
				builder.remove(fcIdx);
			}

			final int itIdx = builder.indexOfType(IdleTimeoutFilter.class);
			if (itIdx != -1) {
				builder.remove(itIdx);
			}
		}
	}
}