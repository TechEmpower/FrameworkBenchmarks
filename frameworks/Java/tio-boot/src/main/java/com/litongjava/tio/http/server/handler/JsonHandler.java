package com.litongjava.tio.http.server.handler;

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
public class JsonHandler implements HttpRequestHandler {

  private static final String HELLO_WORLD = "Hello, World!";
  private static final byte[] JSON_BYTES = JSON.toJSONBytes(new Message(HELLO_WORLD));

  @Override
  public HttpResponse handle(HttpRequest httpRequest) throws Exception {
    HttpResponse response = TioRequestContext.getResponse();
    response.setBody(JSON_BYTES);
    String mimeTypeStr = MimeTypeUtils.getJsonUTF8();
    response.setContentType(mimeTypeStr);
    return response;
  }
}