package com.javanut.gl.benchmark;


import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.PoolOptions;

public class PoolManager {

	private final transient PgConnectOptions options;
	private transient PoolOptions poolOptions;
	private PgPool pool;
	private Vertx vertx;
	
	public PoolManager(PgConnectOptions options, PoolOptions poolOptions) {
		this.options = options;
		this.poolOptions = poolOptions;
		
		this.vertx = Vertx.vertx(new VertxOptions()
				  .setPreferNativeTransport(true)
				  .setWorkerPoolSize(4)//limit threads for this track
				);
		
		boolean usingNative = vertx.isNativeTransportEnabled();
		System.out.println("Running with native: " + usingNative);
	}
		
	public PgPool pool() {
		if (null==pool) {			
			pool = PgPool.pool(vertx, options, poolOptions);			
		}
		return pool;
	}
	
	
}
