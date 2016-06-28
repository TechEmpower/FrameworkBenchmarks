package hello;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;

class Utils {

	private static final ThreadLocal<Context> threadContext = new ThreadLocal<Context>() {
		@Override
		protected Context initialValue() {
			return new Context();
		}
	};

	static Context getContext() throws IOException {
		Context ctx = threadContext.get();
		ctx.json.reset();
		return ctx;
	}

	public static int parseBoundParam(HttpServletRequest request) {
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
