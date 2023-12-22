package com.litongjava.tio.http.server.model;

public final class Fortune {

  public Integer id;
  public String message;

  public Fortune() {
  }

  public Fortune(Integer id, String message) {
    this.id = id;
    this.message = message;
  }

  public Integer getId() {
    return id;
  }

  public String getMessage() {
    return message;
  }
}