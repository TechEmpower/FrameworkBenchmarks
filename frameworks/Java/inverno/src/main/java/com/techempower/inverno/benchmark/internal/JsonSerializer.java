package com.techempower.inverno.benchmark.internal;

import com.dslplatform.json.DslJson;
import com.dslplatform.json.JsonWriter;
import io.inverno.core.annotation.Bean;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import java.lang.reflect.Type;

public class JsonSerializer {

	private static final DslJson<Object> DSL_JSON = new DslJson<>();

	private final JsonWriter jsonWriter;

	public JsonSerializer() {
		this.jsonWriter = DSL_JSON.newWriter();
	}

	public <T> ByteBuf serialize(T value, Type type) {
		try {
			DSL_JSON.serialize(this.jsonWriter, type, value);
			return Unpooled.wrappedBuffer(this.jsonWriter.toByteArray());
		}
		finally {
			this.jsonWriter.reset();
		}
	}

	@Bean( name = "jsonSerializer", visibility = Bean.Visibility.PRIVATE )
	public static class ReactorScope extends io.inverno.mod.base.concurrent.ReactorScope<JsonSerializer> {

		@Override
		protected JsonSerializer create() {
			return new JsonSerializer();
		}
	}
}
