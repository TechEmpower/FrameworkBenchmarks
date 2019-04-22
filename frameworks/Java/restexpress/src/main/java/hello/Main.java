package hello;

import org.restexpress.RestExpress;
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

	public static void main(String[] args) throws Exception {
		Configuration config = Environment.load(args, Configuration.class);
		RestExpress server = new RestExpress().setName("RestExpress")
				.setExecutorThreadCount(config.getExecutorThreadPoolSize())
				.setUseTcpNoDelay(true)
				.setKeepAlive(true)
				.alias("HelloWorld", HelloWorld.class);
		// TODO zloster
		// response compression takes time... AND it's forbidden by the rules
		// .noCompression();

		server.uri("/restexpress/json", config.getJsonController()).action("helloWorld",
				HttpMethod.GET);

		server.uri("/restexpress/mysql/db", config.getMysqlController()).method(HttpMethod.GET);
		server.uri("/restexpress/mysql/query", config.getQueriesMysqlController()).method(HttpMethod.GET);

		server.uri("/restexpress/mongodb/db", config.getMongodbController()).method(HttpMethod.GET);
		server.uri("/restexpress/mongodb/query", config.getQueriesMongodbController()).method(HttpMethod.GET);

		server.bind(config.getPort());
		server.awaitShutdown();
	}
}