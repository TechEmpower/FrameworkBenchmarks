package fi.markoa.tfb.servlet3;

public class World {
  private int id;
  private int randomNumber;

  public World(int id, int randomNumber) {
    this.id = id;
    this.randomNumber = randomNumber;
  }

  public int getId() {
    return id;
  }

  @Override
  public String toString() {
    return "World{" +
      "id=" + id +
      ", randomNumber=" + randomNumber +
      '}';
  }

  public int getRandomNumber() {
    return randomNumber;
  }
}
