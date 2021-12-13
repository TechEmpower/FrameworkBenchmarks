package com.simplyti.cloud.server.benchmark.tests;

import com.simplyti.service.api.builder.ApiBuilder;
import com.simplyti.service.api.builder.ApiProvider;

public class JsonSerialization implements ApiProvider {

	@Override
	public void build(ApiBuilder builder) {
		builder.when()
			.get("/json")
			.then(ctx->ctx.send(new JsonResponse("Hello, World!")));
	}

}
