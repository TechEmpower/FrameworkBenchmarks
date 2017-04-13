package com.techempower;

import io.requery.Entity;
import io.requery.Generated;
import io.requery.Key;

@Entity
public abstract class AbstractWorld {

  @Key
  @Generated
  int id;

  int randomNumber;
}
