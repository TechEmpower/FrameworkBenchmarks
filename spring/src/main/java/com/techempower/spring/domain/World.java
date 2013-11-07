package com.techempower.spring.domain;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

@Entity
public class World {
	@Id
	@GeneratedValue
	public int id;
	public int randomNumber;
}
