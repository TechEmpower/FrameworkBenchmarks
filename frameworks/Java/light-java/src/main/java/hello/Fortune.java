package hello;

/**
 * The model for the "fortune" database table.
 */
public final class Fortune implements Comparable<Fortune> {
  public int id;
  public String message;

  /**
   * Constructs a new fortune object with the given parameters.
   *
   * @param id the ID of the fortune
   * @param message the message of the fortune
   */
  public Fortune(int id, String message) {
    this.id = id;
    this.message = message;
  }

  @Override
  public int compareTo(Fortune other) {
    return message.compareTo(other.message);
  }
}
