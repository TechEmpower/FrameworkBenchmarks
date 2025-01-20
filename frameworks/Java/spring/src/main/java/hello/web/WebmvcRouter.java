package hello.web;

import java.util.Optional;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.function.HandlerFunction;
import org.springframework.web.servlet.function.RouterFunction;
import org.springframework.web.servlet.function.ServerResponse;

@Configuration
public class WebmvcRouter {

	@Bean
	public RouterFunction<ServerResponse> route(
			TextHandler textHandler,
			JsonHandler jsonHandler,
			DbHandler dbHandler) {

		return request -> Optional.of((HandlerFunction<ServerResponse>) r ->
				switch (r.uri().getRawPath()) {
					case "/plaintext" -> textHandler.handle(r);
					case "/json" -> jsonHandler.handle(r);
					case "/db" -> dbHandler.db(r);
					case "/queries" -> dbHandler.queries(r);
					case "/updates" -> dbHandler.updates(r);
					case "/fortunes" -> dbHandler.fortunes(r);
					default -> ServerResponse.notFound().build();
				});
	}
}

