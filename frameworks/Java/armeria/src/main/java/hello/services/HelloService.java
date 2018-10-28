package hello.services;

import hello.models.Message;
import hello.helpers.HttpHeadersHelper;

import java.nio.charset.StandardCharsets;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.core.JsonProcessingException;

import com.linecorp.armeria.common.HttpData;
import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.common.MediaType;
import com.linecorp.armeria.server.annotation.Get;
import com.linecorp.armeria.server.annotation.ProducesJson;

public class HelloService {
  private static final byte[] PLAINTEXT =
      "Hello, World!".getBytes(StandardCharsets.UTF_8);
  private static final ObjectMapper MAPPER = new ObjectMapper();

  @Get("/plaintext")
  public HttpResponse plaintext() {
    return HttpResponse.of(
        HttpHeadersHelper.getHttpHeader(MediaType.PLAIN_TEXT_UTF_8),
        HttpData.of(PLAINTEXT));
  }

  @Get("/json")
  @ProducesJson
  public HttpResponse json() throws JsonProcessingException {
    return HttpResponse.of(
        HttpHeadersHelper.getHttpHeader(MediaType.JSON_UTF_8),
        HttpData.of(MAPPER.writeValueAsBytes(new Message("Hello, World!"))));
  }
}
