package hello.controller;

import org.restexpress.Request;
import org.restexpress.Response;

public class JsonController {

	public HelloWorld sayHello(Request request, Response response) {
		return new HelloWorld();
	}

	/**
	 * Inner class just to illustrate JSON serialization.
	 */
	public static class HelloWorld {
		@SuppressWarnings("unused")
		private String message = "Hello, World!";
	}
}
