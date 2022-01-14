package com.test.hserver.bean;

public class World implements Comparable<World> {

  public final int id;

  public final String message;

  public Fortune(int id, String message) {
    this.id = id;
    this.message = message;
  }
  @Override
  public int compareTo(Fortune other) {
    return message.compareTo(other.message);
  }

  public int getId() {
    return id;
  }

  public String getMessage() {
    return message;
  }
}