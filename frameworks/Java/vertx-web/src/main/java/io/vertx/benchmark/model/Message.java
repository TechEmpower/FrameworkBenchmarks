package io.vertx.benchmark.model;

import io.vertx.core.json.JsonObject;

public class Message extends JsonObject {

  private static final String MESSAGE = "message";

  public Message(String message) {
    put(MESSAGE, message);
  }

  public String getMessage() {
    return getString(MESSAGE);
  }
}
