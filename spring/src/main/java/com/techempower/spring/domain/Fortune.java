package com.techempower.spring.domain;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

@Entity
public class Fortune
		implements Comparable<Fortune> {
	@Id
	@GeneratedValue
	public int id;
	public String message;

	public Fortune() {

	}

	public Fortune(int id, String message) {
		this.id = id;
		this.message = message;
	}

	public int getId() {
		return this.id;
	}

	public String getMessage() {
		return this.message;
	}

	/**
	 * For our purposes, Fortunes sort by their message text.
	 */
	@Override
	public int compareTo(Fortune other) {
		return message.compareTo(other.message);
	}
}
