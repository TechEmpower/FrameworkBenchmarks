package net.officefloor.benchmark;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.CancelledKeyException;
import java.nio.channels.ClosedChannelException;

import com.fasterxml.jackson.databind.ObjectMapper;

import net.officefloor.server.RequestHandler;
import net.officefloor.server.http.HttpResponse;
import net.officefloor.server.http.impl.ProcessAwareServerHttpConnectionManagedObject;
import net.officefloor.server.http.parse.HttpRequestParser;

/**
 * Sends the Update response.
 * 
 * @author Daniel Sagenschneider
 */
public class UpdateSendResponse extends AbstractSendResponse {

	public UpdateSendResponse(RequestHandler<HttpRequestParser> requestHandler,
			ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection, ObjectMapper objectMapper) {
		super(requestHandler, connection, objectMapper);
	}

	public void sendUpdate(World[] worlds) {
		this.requestHandler.execute(() -> {
			try {
				HttpResponse response = this.connection.getResponse();
				response.setContentType(APPLICATION_JSON, null);
				this.objectMapper.writeValue(response.getEntityWriter(), worlds);
				send(this.connection);
			} catch (CancelledKeyException | ClosedChannelException ex) {
				// Ignore as disconnecting client
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		});
	}

}