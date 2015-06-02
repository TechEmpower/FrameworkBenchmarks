package hello;

import org.rapidoid.net.Serve;

public class HelloWebServer {

	public static void main(String[] args) throws Exception {
		int port = args.length > 0 ? Integer.parseInt(args[0]) : 8080;
		Serve.server().protocol(new SimpleHttpProtocol()).port(port).build().start();
	}

}
