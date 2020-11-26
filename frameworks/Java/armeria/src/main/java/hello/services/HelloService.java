package hello.services;

import java.nio.charset.StandardCharsets;

import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.common.HttpStatus;
import com.linecorp.armeria.common.MediaType;
import com.linecorp.armeria.server.annotation.Get;
import com.linecorp.armeria.server.annotation.ProducesJson;

import hello.models.Message;

public class HelloService {
  private static final byte[] PLAINTEXT =
      "Hello, World!".getBytes(StandardCharsets.UTF_8);

  @Get("/plaintext")
  public HttpResponse plaintext() {
    return HttpResponse.of(HttpStatus.OK, MediaType.PLAIN_TEXT_UTF_8, PLAINTEXT);
  }

  @Get("/json")
  @ProducesJson
  public Message json() {
    return new Message("Hello, World!");
  }
}
