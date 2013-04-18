
package com.example.helloworld.core;

import javax.persistence.*;

@Entity
@Table(name = "World")
public class World
{
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private long id;

  @Column(name = "randomNumber", nullable = false)
  private long randomNumber;

  public long getId()
  {
    return id;
  }

  public void setId(long id)
  {
    this.id = id;
  }

  public long getRandomNumber()
  {
    return this.randomNumber;
  }

  public void setRandomNumber(long randomNumber)
  {
    this.randomNumber = randomNumber;
  }
}
