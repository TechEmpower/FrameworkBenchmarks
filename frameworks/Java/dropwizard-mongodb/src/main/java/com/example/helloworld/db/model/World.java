
package com.example.helloworld.db.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.mongojack.Id;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

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
