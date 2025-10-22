package com.techempower.benchmark.pippo;

import ro.pippo.core.ParameterValue;
import ro.pippo.core.route.RouteContext;

import java.util.concurrent.ThreadLocalRandom;

public class BenchmarkUtils {

	// stuff missing from HttpConstants.Header
	public static final class Header {
		public static final String SERVER = "Server";
	}

	public static int random() {
		return 1 + ThreadLocalRandom.current().nextInt(10_000);
	}

	public static int getQueriesParam(RouteContext routeContext) {

		ParameterValue param = routeContext.getParameter("queries");

		if (param.isEmpty())
			return 1;

		int queries;
		try {
			queries = Integer.parseInt(param.toString());
		} catch (NumberFormatException e) {
			return 1;
		}

		if (queries < 1)
			return 1;

		if (queries > 500)
			return 500;

		return queries;

	}

}
