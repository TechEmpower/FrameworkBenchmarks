package com.techempower.inverno.benchmark.model;

public final class World implements Comparable<World> {

	private final int id;
	private int randomNumber;

	public World(int id) {
		this.id = id;
	}
	
	public World(int id, int randomNumber) {
		this.id = id;
		this.randomNumber = randomNumber;
	}
	
	public int getId() {
		return this.id;
	}

	public int getRandomNumber() {
		return this.randomNumber;
	}
	
	public void setRandomNumber(int randomNumber) {
		this.randomNumber = randomNumber;
	}

	@Override
	public int compareTo(World o) {
		return Integer.compare(id, o.id);
	}
}