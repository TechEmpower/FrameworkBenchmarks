package com.techempower;

public final class Fortune implements Comparable<Fortune> {
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
}
