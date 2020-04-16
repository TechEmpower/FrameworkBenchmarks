package com.simplyti.cloud.server.benchmark;

import java.util.concurrent.ExecutionException;

import com.google.inject.AbstractModule;
import com.simplyti.cloud.server.benchmark.tests.JsonSerialization;
import com.simplyti.cloud.server.benchmark.tests.Plaintext;
import com.simplyti.service.builder.di.guice.GuiceService;

public class Main extends  AbstractModule {
	
	public static void main(String...strings) throws InterruptedException, ExecutionException {
		GuiceService.builder()
			.withLog4J2Logger()
			.withName("simple-server")
			.withApi(JsonSerialization.class)
			.withApi(Plaintext.class)
			.build().start().get()
			.stopFuture().await();
	}
	
}
