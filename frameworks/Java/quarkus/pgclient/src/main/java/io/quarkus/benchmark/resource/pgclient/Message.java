package io.quarkus.benchmark.resource.pgclient;

public class Message {
  private final String message;

  public Message(String message) {
    this.message = message;
  }

  public String getMessage() {
    return message;
  }
}
