package hello.services;

import hello.models.Message;

import java.nio.charset.StandardCharsets;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.core.JsonProcessingException;

import com.linecorp.armeria.common.HttpData;
import com.linecorp.armeria.common.HttpHeaderNames;
import com.linecorp.armeria.common.HttpHeaders;
import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.common.HttpStatus;
import com.linecorp.armeria.common.MediaType;
import com.linecorp.armeria.server.annotation.Get;
import com.linecorp.armeria.server.annotation.ProducesJson;

public class HelloService {

  private static final byte[] PLAINTEXT =
      "Hello, World!".getBytes(StandardCharsets.UTF_8);
  private static final ObjectMapper MAPPER = new ObjectMapper();

  @Get("/plaintext")
  public HttpResponse plaintext() {
    HttpHeaders headers = HttpHeaders
        .of(HttpStatus.OK)
        .add(HttpHeaderNames.SERVER, "armeria")
        .add(HttpHeaderNames.DATE,
             DateTimeFormatter.RFC_1123_DATE_TIME
                 .format(ZonedDateTime.now(ZoneOffset.UTC)))
        .contentType(MediaType.PLAIN_TEXT_UTF_8);

    return HttpResponse.of(headers, HttpData.of(PLAINTEXT));
  }

  @Get("/json")
  @ProducesJson
  public HttpResponse json() throws Exception {
    try {
      HttpHeaders headers = HttpHeaders
          .of(HttpStatus.OK)
          .add(HttpHeaderNames.SERVER, "armeria")
          .add(HttpHeaderNames.DATE,
               DateTimeFormatter.RFC_1123_DATE_TIME
                   .format(ZonedDateTime.now(ZoneOffset.UTC)))
          .contentType(MediaType.JSON_UTF_8);

      return HttpResponse.of(
          headers,
          HttpData.of(MAPPER.writeValueAsBytes(new Message("Hello, World!"))));
    } catch (JsonProcessingException e) {
      return HttpResponse.of(HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }
}
