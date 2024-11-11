package benchmark.web;

import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import reactor.core.publisher.Mono;

import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.HandlerFunction;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;

@Component
public class JsonHandler implements HandlerFunction<ServerResponse> {

	private final ObjectWriter writer;

	public JsonHandler(ObjectMapper objectMapper) {
		this.writer = objectMapper.writerFor(Map.class);
	}

	@Override
	public Mono<ServerResponse> handle(ServerRequest request) {
		return ServerResponse.ok()
				.body((message, context) -> {
					DataBuffer buffer = serialize(request, Map.of("message", "Hello, world!"));
					HttpHeaders headers = message.getHeaders();
					headers.setContentLength(buffer.readableByteCount());
					headers.add(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE);
					return message.writeWith(Mono.just(buffer));
				});
	}

	private DataBuffer serialize(ServerRequest request, Object object) {
		try {
			byte[] bytes = this.writer.writeValueAsBytes(object);
			return bufferFactory(request).wrap(bytes);
		}
		catch (JsonProcessingException ex) {
			throw new RuntimeException(ex);
		}
	}

	private static DataBufferFactory bufferFactory(ServerRequest request) {
		return request.exchange().getResponse().bufferFactory();
	}

}