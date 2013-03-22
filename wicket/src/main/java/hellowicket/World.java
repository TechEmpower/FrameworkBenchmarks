package hellowicket;

import java.util.*;

import javax.persistence.*;

@Entity
public class World
{
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  public int id;
  public int randomNumber;
}
