package hello;

import com.dslplatform.json.JsonWriter;

import javax.servlet.http.HttpServletRequest;

abstract class Utils {

	private static final ThreadLocal<Context> threadContext = ThreadLocal.withInitial(Context::new);
	private static final ThreadLocal<JsonWriter> threadJson = ThreadLocal.withInitial(JsonWriter::new);

	static Context getContext() {
		return threadContext.get();
	}

	static JsonWriter getJson() {
		JsonWriter json = threadJson.get();
		json.reset();
		return json;
	}

	static int parseBoundParam(HttpServletRequest request) {
		int count = 1;
		try {
			count = Integer.parseInt(request.getParameter("queries"));
			if (count > 500) {
				count = 500;
			} else if (count < 1) {
				count = 1;
			}
		} catch (NumberFormatException ignore) {
		}
		return count;
	}
}
