package hello.domain;

import java.util.*;

import javax.persistence.*;

@Entity
public class Fortune
  implements Comparable<Fortune>
{
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  public int id;
  public String message;
  
  public Fortune(int id, String message)
  {
    this.id = id;
    this.message = message;
  }
  
  /**
   * For our purposes, Fortunes sort by their message text. 
   */
  @Override
  public int compareTo(Fortune other)
  {
    return message.compareTo(other.message);
  }
}
