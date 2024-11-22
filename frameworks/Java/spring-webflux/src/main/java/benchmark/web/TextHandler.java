package benchmark.web;

import java.nio.charset.StandardCharsets;

import reactor.core.publisher.Mono;

import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.HandlerFunction;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;

@Component
public class TextHandler implements HandlerFunction<ServerResponse> {

	private static final byte[] TEXT_BODY = "Hello, World!".getBytes(StandardCharsets.UTF_8);

	private static final String TEXT_BODY_LENGTH = String.valueOf(TEXT_BODY.length);

	@Override
	public Mono<ServerResponse> handle(ServerRequest request) {
		return ServerResponse.ok()
				.body((message, context) -> {
					HttpHeaders headers = message.getHeaders();
					headers.add(HttpHeaders.CONTENT_LENGTH, TEXT_BODY_LENGTH);
					headers.add(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN_VALUE);
					return message.writeWith(Mono.just(bufferFactory(request).wrap(TEXT_BODY)));
				});
	}

	private static DataBufferFactory bufferFactory(ServerRequest request) {
		return request.exchange().getResponse().bufferFactory();
	}

}