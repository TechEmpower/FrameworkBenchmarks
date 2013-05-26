package hello;

import hello.config.Configuration;
import hello.controller.JsonController.HelloWorld;

import java.io.FileNotFoundException;
import java.io.IOException;

import org.jboss.netty.handler.codec.http.HttpMethod;

import com.strategicgains.restexpress.RestExpress;
import com.strategicgains.restexpress.util.Environment;

public class Main
{
	public static void main(String[] args) throws Exception
	{
		Configuration config = loadEnvironment(args);
		RestExpress server = new RestExpress()
			.setName("RestExpress Benchmark")
		    .setExecutorThreadCount(config.getExecutorThreadPoolSize())
		    .alias("HelloWorld", HelloWorld.class);

		server.uri("/restexpress/json", config.getJsonController())
			.action("helloWorld", HttpMethod.GET);

		server.uri("/restexpress/mysql", config.getMysqlController())
			.method(HttpMethod.GET);

		server.uri("/restexpress/mongodb", config.getMongodbController())
		    .method(HttpMethod.GET);

		server.bind(config.getPort());
		server.awaitShutdown();
	}

	private static Configuration loadEnvironment(String[] args)
	throws FileNotFoundException,
	    IOException
	{
		if (args.length > 0)
		{
			return Environment.from(args[0], Configuration.class);
		}

		return Environment.fromDefault(Configuration.class);
	}
}
