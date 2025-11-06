package com.litongjava.tio.http.server.handler;

import java.nio.charset.StandardCharsets;

import com.alibaba.fastjson2.JSON;
import com.litongjava.tio.boot.http.TioRequestContext;
import com.litongjava.tio.http.common.HttpRequest;
import com.litongjava.tio.http.common.HttpResponse;
import com.litongjava.tio.http.common.utils.MimeTypeUtils;
import com.litongjava.tio.http.server.model.Message;

/**
 * ab -k -n1000000 -c10 http://127.0.0.1:8080/json ab -k -n1000000 -c10
 * http://127.0.0.1:8080/plaintext
 */
public class IndexHandler {

  private static final String HELLO_WORLD = "Hello, World!";

  private static final byte[] HELLO_WORLD_BYTES = HELLO_WORLD.getBytes(StandardCharsets.UTF_8);
  private static final byte[] JSON_BYTES = JSON.toJSONBytes(new Message(HELLO_WORLD));

  public HttpResponse plaintext(HttpRequest request) {
    HttpResponse response = TioRequestContext.getResponse();
    response.setBody(HELLO_WORLD_BYTES);
    String mimeTypeStr = MimeTypeUtils.getTextUTF8();
    response.setContentType(mimeTypeStr);
    return response;
  }

  public HttpResponse json(HttpRequest request) {
    HttpResponse response = TioRequestContext.getResponse();
    response.setBody(JSON_BYTES);
    String mimeTypeStr = MimeTypeUtils.getJsonUTF8();
    response.setContentType(mimeTypeStr);
    return response;
  }
}