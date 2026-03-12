package io.tadx.benchmark;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;


/**
 * The main program loads the configuration and starts the server by `tadx-web` <br/>
 *
 * @author Tad.x team
 * @since 1.0.0
 */
@SpringBootApplication
public class Application { // extends VerticleBase implements Handler<HttpServerRequest>

    public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}
}
