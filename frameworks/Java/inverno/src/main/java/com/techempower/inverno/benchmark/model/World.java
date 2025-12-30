package com.techempower.inverno.benchmark.model;

import com.dslplatform.json.CompiledJson;

@CompiledJson
public final class World implements Comparable<World> {

	private int id;
	private int randomNumber;

	public World() {}

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