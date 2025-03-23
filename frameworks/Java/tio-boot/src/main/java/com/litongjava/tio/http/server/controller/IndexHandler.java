package com.litongjava.tio.http.server.controller;

import com.alibaba.fastjson2.JSON;
import com.litongjava.tio.boot.http.TioRequestContext;
import com.litongjava.tio.http.common.HeaderName;
import com.litongjava.tio.http.common.HeaderValue;
import com.litongjava.tio.http.common.HttpRequest;
import com.litongjava.tio.http.common.HttpResponse;
import com.litongjava.tio.http.server.model.Message;
import com.litongjava.tio.http.server.util.Resps;

/**
 * ab -k -n1000000 -c10 http://127.0.0.1:8080/json 
 * ab -k -n1000000 -c10 http://127.0.0.1:8080/plaintext
 */
public class IndexHandler {
  private static final String HELLO_WORLD = "Hello, World!";

  private static final byte[] HELLO_WORLD_BYTES = HELLO_WORLD.getBytes();

  public HttpResponse index(HttpRequest request) {
    return Resps.txt(request, "tio-boot");
  }

  public HttpResponse plaintext(HttpRequest request) {
    HttpResponse response = TioRequestContext.getResponse();
    response.setBody(HELLO_WORLD_BYTES);
    response.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_TXT);
    return response;
  }

  public HttpResponse json(HttpRequest request) {
    HttpResponse response = TioRequestContext.getResponse();
    response.setBody(JSON.toJSONBytes(new Message(HELLO_WORLD)));
    response.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_JSON);
    return response;
  }
}