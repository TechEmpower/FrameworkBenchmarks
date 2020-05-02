package com.simplyti.cloud.server.benchmark.tests;

import com.simplyti.service.api.builder.ApiBuilder;
import com.simplyti.service.api.builder.ApiProvider;

public class Plaintext implements ApiProvider {

	@Override
	public void build(ApiBuilder builder) {
		builder.when()
			.get("/plaintext")
			.then(ctx->ctx.send("Hello, World!"));
	}

}
