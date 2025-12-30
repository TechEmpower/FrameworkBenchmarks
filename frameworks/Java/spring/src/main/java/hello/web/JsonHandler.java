package hello.web;

import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;

import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.function.HandlerFunction;
import org.springframework.web.servlet.function.ServerRequest;
import org.springframework.web.servlet.function.ServerResponse;

@Component
public class JsonHandler implements HandlerFunction<ServerResponse> {

	private final ObjectWriter writer;

	public JsonHandler(ObjectMapper objectMapper) {
		this.writer = objectMapper.writerFor(Map.class);
	}

	@Override
	public ServerResponse handle(ServerRequest request) {
		try {
			byte[] body = this.writer.writeValueAsBytes(Map.of("message", "Hello, world!"));
			return ServerResponse.ok()
					.contentLength(body.length)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
					.body(body);
		}
		catch (JsonProcessingException ex) {
			throw new RuntimeException(ex);
		}
	}

}
