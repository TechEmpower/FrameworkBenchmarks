package net.officefloor.benchmark;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.ByteBuffer;
import java.nio.channels.CancelledKeyException;
import java.nio.channels.ClosedChannelException;

import com.fasterxml.jackson.databind.ObjectMapper;

import net.officefloor.server.RequestHandler;
import net.officefloor.server.http.HttpHeaderValue;
import net.officefloor.server.http.HttpResponse;
import net.officefloor.server.http.HttpStatus;
import net.officefloor.server.http.impl.ProcessAwareServerHttpConnectionManagedObject;
import net.officefloor.server.http.parse.HttpRequestParser;

/**
 * Handles sending {@link HttpResponse}.
 * 
 * @author Daniel Sagenschneider
 */
public class AbstractSendResponse {

	public static final HttpHeaderValue TEXT_PLAIN = new HttpHeaderValue("text/plain");

	public static final HttpHeaderValue APPLICATION_JSON = new HttpHeaderValue("application/json");

	public static void send(ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection) throws IOException {
		try {
			connection.getServiceFlowCallback().run(null);
		} catch (IOException ex) {
			throw ex;
		} catch (Throwable ex) {
			throw new IOException(ex);
		}
	}

	protected final RequestHandler<HttpRequestParser> requestHandler;

	protected final ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection;

	protected final ObjectMapper objectMapper;

	public AbstractSendResponse(RequestHandler<HttpRequestParser> requestHandler,
			ProcessAwareServerHttpConnectionManagedObject<ByteBuffer> connection, ObjectMapper objectMapper) {
		this.requestHandler = requestHandler;
		this.connection = connection;
		this.objectMapper = objectMapper;
	}

	public void sendOverloaded() {
		try {
			HttpResponse response = this.connection.getResponse();
			response.reset();

			// Send overloaded
			response.setStatus(HttpStatus.SERVICE_UNAVAILABLE);
			send(this.connection);

		} catch (CancelledKeyException | ClosedChannelException ex) {
			// Ignore as disconnecting client
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}

	public void sendError(Throwable failure) {
		try {
			HttpResponse response = this.connection.getResponse();
			response.reset();

			// Send failure
			response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR);
			response.setContentType(TEXT_PLAIN, null);
			failure.printStackTrace(new PrintWriter(response.getEntityWriter()));
			send(this.connection);

		} catch (CancelledKeyException | ClosedChannelException ex) {
			// Ignore as disconnecting client
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}

	public void sendError(int status) {
		try {
			// Setup to send response
			HttpResponse response = connection.getResponse();
			response.reset();

			// Send error response
			response.setStatus(HttpStatus.getHttpStatus(status));
			send(this.connection);

		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}

}