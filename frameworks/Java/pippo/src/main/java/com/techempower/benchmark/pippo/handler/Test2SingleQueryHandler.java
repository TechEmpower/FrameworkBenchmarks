package com.techempower.benchmark.pippo.handler;

import com.dslplatform.json.DslJson;
import com.techempower.benchmark.pippo.BenchmarkUtils;
import com.techempower.benchmark.pippo.dao.Dao;
import com.techempower.benchmark.pippo.model.World;
import ro.pippo.core.HttpConstants;
import ro.pippo.core.route.RouteContext;
import ro.pippo.core.route.RouteHandler;

import java.io.IOException;

/**
 * <p>Test type 2: Single database query</p>
 *
 * <p>Example request:</p>
 *
 * <pre>
 * GET /db HTTP/1.1
 * Host: server
 * User-Agent: Mozilla/5.0 (X11; Linux x86_64) Gecko/20130501 Firefox/30.0 AppleWebKit/600.00 Chrome/30.0.0000.0 Trident/10.0 Safari/600.00
 * Cookie: uid=12345678901234567890; __utma=1.1234567890.1234567890.1234567890.1234567890.12; wd=2560x1600
 * Accept: text/html,application/xhtml+xml,application/xml;q=0.9,"*&#47;"/"*&#47;";q=0.8
 * Accept-Language: en-US,en;q=0.5
 * Connection: keep-alive
 * </pre>
 *
 * <p>Example response:</p>
 *
 * <pre>
 * HTTP/1.1 200 OK
 * Content-Length: 32
 * Content-Type: application/json
 * Server: Example
 * Date: Wed, 17 Apr 2013 12:00:00 GMT
 *
 * {"id":3217,"randomNumber":2149}
 * </pre>
 */
public class Test2SingleQueryHandler implements RouteHandler<RouteContext> {

	public Test2SingleQueryHandler(Dao dao) {
		this.dao = dao;
	}

	@Override
	public void handle(RouteContext routeContext) {
		try {
			WorldJsonMapper.serialize(
				dao.getRandomWorld(),
				routeContext
					.getResponse()
					.contentType(HttpConstants.ContentType.APPLICATION_JSON)
					.header(BenchmarkUtils.Header.SERVER, "Pippo")
					.getOutputStream()
			);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	private static final DslJson<World> WorldJsonMapper = new DslJson<>();

	private final Dao dao;

}