package com.techempower;

import com.dslplatform.json.CompiledJson;

@CompiledJson
public class Message {
  private String message;

  public Message(String message) {
    this.message = message;
  }

  public String getMessage() {
    return message;
  }
}
