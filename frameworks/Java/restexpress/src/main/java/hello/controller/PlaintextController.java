package hello.controller;

import org.restexpress.Request;
import org.restexpress.Response;

public class PlaintextController
{
	private static final String MESSAGE = "Hello, World!";

	public String sayHello(Request request, Response response)
	{
		response.setContentType("text/plain");
		return MESSAGE;
	}
}
