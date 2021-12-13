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
import io.inverno.mod.sql.vertx.PooledClientSqlClient;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.PoolOptions;

@Bean( name = "pooledClientSqlClient", visibility = Visibility.PRIVATE )
public class PooledClientSqlClientReactorScope extends ReactorScope<SqlClient> {

	private final AppConfiguration configuration;
	private final Reactor reactor;
	
	private Vertx vertx;
	private PgConnectOptions connectOptions;
	private PoolOptions poolOptions;
	
	public PooledClientSqlClientReactorScope(AppConfiguration configuration, Reactor reactor) {
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
				.setCachePreparedStatements(true);
		
		this.poolOptions = new PoolOptions().setMaxSize(1);
	}
	
	@Destroy
	public void destroy() {
		if(!(this.reactor instanceof VertxReactor)) {
			this.vertx.close();			
		}
	}
	
	@Override
	protected SqlClient create() {
		return new PooledClientSqlClient(PgPool.client(this.vertx, this.connectOptions, this.poolOptions));
	}

}
