package hello.domain;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

@Entity
public class Fortune implements Comparable<Fortune> {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  public int id;
  public String message;

  public Fortune() { }

  public Fortune(int id, String message) {
    this.id = id;
    this.message = message;
  }

  @Override
  public int compareTo(Fortune other) {
    return message.compareTo(other.message);
  }
}
