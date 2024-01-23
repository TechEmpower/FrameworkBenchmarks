package com.techempower.benchmark.pippo.model;

import com.dslplatform.json.CompiledJson;

@CompiledJson
public class Fortune {

	public Fortune(int id, String message) {
		this.id = id;
		this.message = message;
	}

	public int id;
	public String message;

}