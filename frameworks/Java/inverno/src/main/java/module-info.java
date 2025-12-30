@io.inverno.core.annotation.Module( excludes = { "io.inverno.mod.sql.vertx" } )
module com.techempower.inverno.benchmark {
	requires io.inverno.mod.boot;
	requires io.inverno.mod.http.server;
	requires io.inverno.mod.irt;
	
	requires io.inverno.mod.sql;
	requires io.inverno.mod.sql.vertx;
	
	requires io.netty.common;
	requires io.netty.codec.http;
	requires unbescape;
	requires static dsl.json;

	requires io.vertx.client.sql.pg;
	requires io.vertx.client.sql;
	requires io.vertx.core;
	requires java.sql;

	exports com.techempower.inverno.benchmark;
	exports com.techempower.inverno.benchmark.model;
}
