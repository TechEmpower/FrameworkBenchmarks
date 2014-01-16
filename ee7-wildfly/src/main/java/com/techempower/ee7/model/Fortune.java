package com.techempower.ee7.model;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@Entity
public class Fortune implements Comparable<Fortune>, Serializable {

  private static final long serialVersionUID = 1L;

  private int id;
  private String message;

  public Fortune() {

  }

  public Fortune(int id, String message) {
    this.id = id;
    this.message = message;
  }

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
  @Size(max = 2048)
  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }

  @Override
  public int compareTo(Fortune o) {
    return message.compareTo(o.getMessage());
  }
}
