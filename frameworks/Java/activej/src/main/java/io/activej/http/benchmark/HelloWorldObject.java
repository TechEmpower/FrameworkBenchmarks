package io.activej.http.benchmark;

import com.dslplatform.json.CompiledJson;

@CompiledJson
public final class HelloWorldObject {
	private final String message;

	public HelloWorldObject(String message) {
		this.message = message;
	}

	public String getMessage() {
		return message;
	}
}
