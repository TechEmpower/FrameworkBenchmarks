package com.example.helloworld.resources;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/plaintext")
@Produces(MediaType.TEXT_PLAIN)
public class TextResource {
	private static final String MESSAGE = "Hello, World!";
	private static final byte[] buffer;

	static {
		try {
			buffer = MESSAGE.getBytes("US-ASCII");
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	@GET
	public byte[] sayHello() {
		return buffer;
	}
}
