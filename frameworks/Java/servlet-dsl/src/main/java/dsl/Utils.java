package dsl;

import com.dslplatform.client.json.JsonWriter;

public class Utils {

	private static final ThreadLocal<JsonWriter> threadWriter = new ThreadLocal<JsonWriter>() {
		@Override
		protected JsonWriter initialValue() {
			return new JsonWriter();
		}
	};

	public static JsonWriter getJson() {
		final JsonWriter writer = threadWriter.get();
		writer.reset();
		return writer;
	}
}
