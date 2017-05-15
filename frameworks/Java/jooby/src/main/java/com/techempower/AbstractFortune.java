package com.techempower;

import io.requery.Entity;
import io.requery.Key;

/**
 * The model for the "fortune" database table.
 */
@Entity
public abstract class AbstractFortune implements Comparable<AbstractFortune> {
  @Key
  int id;

  String message;

  @Override
  public int compareTo(AbstractFortune other) {
    return message.compareTo(other.message);
  }
}
