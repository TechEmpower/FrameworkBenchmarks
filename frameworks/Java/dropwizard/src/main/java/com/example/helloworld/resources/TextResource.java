package com.example.helloworld.resources;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.nio.charset.StandardCharsets;

@Path("/plaintext")
@Produces(MediaType.TEXT_PLAIN)
public class TextResource {
	private static final byte[] MESSAGE = "Hello, World!".getBytes(StandardCharsets.US_ASCII);

	@GET
	public byte[] sayHello() {
		return MESSAGE;
	}
}
