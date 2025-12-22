package com.litongjava.tio.http.server.handler;

import java.nio.charset.StandardCharsets;

import com.litongjava.tio.boot.http.TioRequestContext;
import com.litongjava.tio.http.common.HttpRequest;
import com.litongjava.tio.http.common.HttpResponse;
import com.litongjava.tio.http.common.utils.MimeTypeUtils;

public class PlaintextHandler implements HttpRequestHandler {

  private static final String HELLO_WORLD = "Hello, World!";

  private static final byte[] HELLO_WORLD_BYTES = HELLO_WORLD.getBytes(StandardCharsets.UTF_8);

  @Override
  public HttpResponse handle(HttpRequest httpRequest) throws Exception {
    HttpResponse response = TioRequestContext.getResponse();
    response.setBody(HELLO_WORLD_BYTES);
    String mimeTypeStr = MimeTypeUtils.getTextUTF8();
    response.setContentType(mimeTypeStr);
    return response;
  }

}
