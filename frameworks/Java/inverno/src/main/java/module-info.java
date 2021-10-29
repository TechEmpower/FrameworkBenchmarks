@io.inverno.core.annotation.Module( excludes = { "io.inverno.mod.sql.vertx" } )

@io.inverno.core.annotation.Wire(beans="pooledClientSqlClient", into="handler:pooledClientSqlClient") 
@io.inverno.core.annotation.Wire(beans="poolSqlClient", into="handler:poolSqlClient") 
module com.techempower.inverno.benchmark {
	requires io.inverno.mod.boot;
	requires io.inverno.mod.http.server;
	requires io.inverno.mod.irt;
	
	requires io.inverno.mod.sql;
	requires io.inverno.mod.sql.vertx;
	
	requires io.netty.common;
	requires io.netty.codec.http;
	requires unbescape;
	
	requires io.vertx.client.sql.pg;
	requires io.vertx.client.sql;
	requires io.vertx.core;
	requires java.sql;
	
	requires transitive io.netty.transport;
	requires static io.netty.transport.unix.common;
	requires static io.netty.transport.epoll;
	
	exports com.techempower.inverno.benchmark;
	exports com.techempower.inverno.benchmark.model;
}
