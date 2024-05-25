package com.litongjava.tio.http.server.model;

public final class World {

  public int id;
  public int randomnumber;

  protected World() {
  }

  public World(int id, int randomnumber) {
    this.id = id;
    this.randomnumber = randomnumber;
  }

  public int getId() {
    return id;
  }

  public void setId(int id) {
    this.id = id;
  }

  public int getRandomnumber() {
    return randomnumber;
  }

  public void setRandomnumber(int randomnumber) {
    this.randomnumber = randomnumber;
  }

}