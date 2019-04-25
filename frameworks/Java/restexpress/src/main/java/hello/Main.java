package hello;

import org.restexpress.RestExpress;
import org.restexpress.common.exception.ConfigurationException;
import org.restexpress.util.Environment;

import hello.config.Configuration;
import hello.controller.JsonController.HelloWorld;
import io.netty.handler.codec.http.HttpMethod;
import io.netty.util.ResourceLeakDetector;
import io.netty.util.ResourceLeakDetector.Level;

public class Main {
	static {
		// Netty optimization.
		ResourceLeakDetector.setLevel(Level.DISABLED);
	}

	public static void main(String[] args) throws Throwable {
		Configuration config = Environment.load(args, Configuration.class);
		RestExpress server = new RestExpress().setName("RestExpress")
				.setExecutorThreadCount(config.getExecutorThreadPoolSize()).setUseTcpNoDelay(true)
				.setKeepAlive(true).noCompression().setEnforceHttpSpec(true)
				.alias("HelloWorld", HelloWorld.class)
				.addPostprocessor(new RequiredResponseHeaders());

		switch (config.getDatabase()) {
		case MongoDB: {
			server.uri("/plaintext", config.getPlaintextController()).action("sayHello",
					HttpMethod.GET).noSerialization();
			server.uri("/json", config.getJsonController()).action("sayHello", HttpMethod.GET);
			
			server.uri("/db", config.getMongodbController()).method(HttpMethod.GET);
			server.uri("/query", config.getQueriesMongodbController()).method(HttpMethod.GET);
		}
			break;
		case MySQL: {
			server.uri("/db", config.getMysqlController()).method(HttpMethod.GET);
			server.uri("/query", config.getQueriesMysqlController()).method(HttpMethod.GET);
		}
			break;
		default:
			throw new ConfigurationException(
					"No Database configured in the environment.properties file.");
		}

		server.bind(config.getPort());
		server.awaitShutdown();
	}
}