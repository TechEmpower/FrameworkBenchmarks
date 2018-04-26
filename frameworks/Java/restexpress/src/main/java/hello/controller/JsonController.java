package hello.controller;

import com.strategicgains.restexpress.Request;
import com.strategicgains.restexpress.Response;

public class JsonController
{
	public HelloWorld helloWorld(Request request, Response response)
	{
		return new HelloWorld();
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
