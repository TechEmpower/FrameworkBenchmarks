package com.techempower.ee7.model;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@Entity
public class World implements Serializable {

  private static final long serialVersionUID = 1L;

  private int id;
  private int randomNumber;

  @XmlElement
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  public int getId() {
    return id;
  }

  public void setId(int id) {
    this.id = id;
  }

  @XmlElement
  @NotNull
  public int getRandomNumber() {
    return randomNumber;
  }

  public void setRandomNumber(int randomNumber) {
    this.randomNumber = randomNumber;
  }
}
