package benchmark;
import com.dslplatform.json.DslJson;
import com.dslplatform.json.runtime.Settings;
import io.javalin.http.Context;
import io.javalin.json.JsonMapper;
import org.jetbrains.annotations.NotNull;

import java.io.*;

import java.lang.reflect.Type;
import java.util.Objects;

public class CustomJsonMapper implements JsonMapper {

	public static DslJson<Object> dslJson = new DslJson<>(Settings.withRuntime().allowArrayFormat(true).includeServiceLoader());

	public void writeJson(Object obj, Context context) {
		try {
			dslJson.serialize(obj, context.res().getOutputStream());
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	@NotNull
	@Override
	public <T> T fromJsonStream(@NotNull InputStream json, @NotNull Type targetType) {
		try {
			return (T) Objects.requireNonNull(dslJson.deserialize(targetType, json));
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	@NotNull
	@Override
	public <T> T fromJsonString(@NotNull String json, @NotNull Type targetType) {
		byte[] bytes = json.getBytes();
		Object res = null;
		try {
			res = dslJson.deserialize(targetType, bytes, bytes.length);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return (T) Objects.requireNonNull(res);
	}

	@NotNull
	@Override
	public InputStream toJsonStream(@NotNull Object obj, @NotNull Type type) {
		PipedInputStream in = new PipedInputStream();
		PipedOutputStream out = null;
		try {
			out = new PipedOutputStream(in);
			dslJson.serialize(obj, out);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return in;
	}

	@NotNull
	@Override
	public String toJsonString(@NotNull Object obj, @NotNull Type type) {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		try {
			dslJson.serialize(obj,os);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return os.toString();
	}

}
