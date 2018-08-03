package hello.controller;

import com.strategicgains.restexpress.Request;
import com.strategicgains.restexpress.Response;

public class PlaintextController
{
	private static final String MESSAGE = "Hello, World!";

	public String helloWorld(Request request, Response response)
	{
		response.setContentType("text/plain");
		return MESSAGE;
	}
}
