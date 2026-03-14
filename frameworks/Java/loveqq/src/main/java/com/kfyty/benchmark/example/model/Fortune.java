package com.kfyty.benchmark.example.model;

public final class Fortune implements Comparable<Fortune>{
	public int id;
	public String message;

	public Fortune() {
	}

	public Fortune(int id, String message) {
		this.id = id;
		this.message = message;
	}

	@Override
	public int compareTo(final Fortune other) {
		return message.compareTo(other.message);
	}
}
