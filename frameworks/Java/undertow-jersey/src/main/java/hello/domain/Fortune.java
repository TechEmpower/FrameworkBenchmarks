package hello.domain;

import javax.persistence.*;

@Entity
public class Fortune
    implements Comparable<Fortune>
{

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  public int    id;
  public String message;

  public Fortune()
  {
  }

  public Fortune(final int id, final String message)
  {
    this.id = id;
    this.message = message;
  }

  @Override
  public int compareTo(final Fortune other)
  {
    return message.compareTo(other.message);
  }
}
