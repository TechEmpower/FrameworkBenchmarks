package hello.entities;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

/**
 * Database Mapping Test entity
 */
@Entity
public class Fortune implements Comparable<Fortune> {
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  public int id;
  public String message;

  @Override
  public int compareTo(final Fortune o) {
    return message.compareTo(o.message);
  }
}
