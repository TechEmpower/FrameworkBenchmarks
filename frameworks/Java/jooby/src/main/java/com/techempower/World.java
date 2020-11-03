package com.techempower;

import com.dslplatform.json.CompiledJson;

@CompiledJson
public class World implements Comparable<World> {

  private int id;

  private int randomNumber;

  public World(int id, int randomNumber) {
    this.id = id;
    this.randomNumber = randomNumber;
  }

  public int getId() {
    return id;
  }

  public int getRandomNumber() {
    return randomNumber;
  }

  @Override public int compareTo(World o) {
    return id - o.id;
  }
}
