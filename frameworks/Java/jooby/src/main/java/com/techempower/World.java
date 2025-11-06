package com.techempower;

import com.dslplatform.json.CompiledJson;

@CompiledJson
public class World implements Comparable<World> {

  private int id;

  private Integer randomNumber;

  public World(int id, Integer randomNumber) {
    this.id = id;
    this.randomNumber = randomNumber;
  }

  public int getId() {
    return id;
  }

  public Integer getRandomNumber() {
    return randomNumber;
  }

  @Override public int compareTo(World o) {
    return id - o.id;
  }
}
