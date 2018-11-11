package hello.entities;

import java.util.*;

import javax.persistence.*;

/**
 * Database Mapping Test entity
 */
@Entity
public class World
{
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  public int id;
  public int randomNumber;
}
