package com.techempower.benchmark.pippo.model;

import com.dslplatform.json.CompiledJson;

@CompiledJson
public class World {

	public World(int id, int randomNumber) {
		this.id = id;
		this.randomNumber = randomNumber;
	}

	public int id;
	public int randomNumber;

}