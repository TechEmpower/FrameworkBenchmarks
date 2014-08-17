package hello.controller;

import com.strategicgains.restexpress.Request;
import com.strategicgains.restexpress.Response;

public class JsonController
{
	private static final HelloWorld MESSAGE_OBJECT = new HelloWorld();

	public HelloWorld helloWorld(Request request, Response response)
	{
		return MESSAGE_OBJECT;
	}

	/**
	 * Inner class just to illustrate JSON serialization.
	 */
	public static class HelloWorld
	{
		@SuppressWarnings("unused")
        private String message = "Hello, World!";
	}
}
