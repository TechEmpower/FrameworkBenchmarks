package net.officefloor.performance.logic;

import java.io.IOException;

import lombok.Data;
import net.officefloor.plugin.json.JsonResponseWriter;

/**
 * Logic for JSON Serialisation.
 * 
 * @author Daniel Sagenschneider
 */
public class JsonSerialisationLogic {

	@Data
	public static class Message {
		private final String message;
	}

	public void service(JsonResponseWriter writer) throws IOException {
		writer.writeResponse(new Message("Hello, World!"));
	}

}