package com.litongjava.aio.http.server.model;

public final class Fortune {

  public Long id;
  public String message;

  public Fortune() {
  }

  public Fortune(Long id, String message) {
    this.id = id;
    this.message = message;
  }

  public Long getId() {
    return id;
  }

  public String getMessage() {
    return message;
  }
}