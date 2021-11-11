package com.techempower.inverno.benchmark.model;

public final class Fortune implements Comparable<Fortune> {

	private final int id;
	private final String message;

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

	@Override
	public int compareTo(Fortune other) {
		return getMessage().compareTo(other.getMessage());
	}
}
