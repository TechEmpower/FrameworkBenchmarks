package hello.models;

public class Fortune {
  public int id;
  public String message;

  public String getMessage() {
    return this.message;
  }

  public Fortune(int id, String message) {
    this.id = id;
    this.message = message;
  }
}
