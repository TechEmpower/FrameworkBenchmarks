package com.javanut.gl.benchmark;

import io.reactiverse.pgclient.PgClient;
import io.reactiverse.pgclient.PgPool;
import io.reactiverse.pgclient.PgPoolOptions;

public class PoolManager {

	private final transient PgPoolOptions options;
	private transient PgPool pool;
	
	public PoolManager(PgPoolOptions options) {
		this.options = options;
		
	}
		
	public PgPool pool() {
		if (null==pool) {			
			pool = PgClient.pool(options);			
		}
		return pool;
	}
	
	
}
