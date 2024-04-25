package com.techempower.inverno.benchmark.internal;

import com.techempower.inverno.benchmark.AppConfiguration;

import io.inverno.core.annotation.Bean;
import io.inverno.core.annotation.Bean.Visibility;
import io.inverno.core.annotation.Destroy;
import io.inverno.core.annotation.Init;
import io.inverno.mod.base.concurrent.Reactor;
import io.inverno.mod.base.concurrent.ReactorScope;
import io.inverno.mod.base.concurrent.VertxReactor;
import io.inverno.mod.sql.SqlClient;
import io.inverno.mod.sql.vertx.ConnectionSqlClient;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgConnection;
import reactor.core.publisher.Mono;

@Bean( name = "SqlClient", visibility = Visibility.PRIVATE )
public class SqlClientReactorScope extends ReactorScope<Mono<SqlClient>> {

	private final AppConfiguration configuration;
	private final Reactor reactor;
	
	private Vertx vertx;
	private PgConnectOptions connectOptions;
	
	public SqlClientReactorScope(AppConfiguration configuration, Reactor reactor) {
		this.configuration = configuration;
		this.reactor = reactor;
	}
	
	@Init
	public void init() {
		if(this.reactor instanceof VertxReactor) {
			this.vertx = ((VertxReactor)this.reactor).getVertx();			
		}
		else {
			this.vertx = Vertx.vertx(new VertxOptions().setPreferNativeTransport(this.configuration.boot().prefer_native_transport()));
		}
		
		this.connectOptions = new PgConnectOptions()
				.setHost(this.configuration.db_host())
				.setPort(this.configuration.db_port())
				.setDatabase(this.configuration.db_database())
				.setUser(this.configuration.db_username())
				.setPassword(this.configuration.db_password())
				.setCachePreparedStatements(true)
				.setPipeliningLimit(100_100);
	}
	
	@Destroy
	public void destroy() {
		if(!(this.reactor instanceof VertxReactor)) {
			this.vertx.close();			
		}
	}
	
	@Override
	protected Mono<SqlClient> create() {
		return Mono.fromCompletionStage(() -> PgConnection.connect(this.vertx, this.connectOptions).toCompletionStage())
			.map(pgConn -> (SqlClient)new ConnectionSqlClient(pgConn))
			.cacheInvalidateWhen(client -> ((ConnectionSqlClient)client).onClose());
	}
}
