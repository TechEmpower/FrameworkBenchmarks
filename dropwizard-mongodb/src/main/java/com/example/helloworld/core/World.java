
package com.example.helloworld.core;

import javax.persistence.*;

import org.mongojack.Id;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@Entity
@Table(name = "World")
@JsonIgnoreProperties(ignoreUnknown = true)
public class World
{
  @Id
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
