package com.ociweb.gl.benchmark;

import io.reactiverse.pgclient.PgClient;
import io.reactiverse.pgclient.PgPool;
import io.reactiverse.pgclient.PgPoolOptions;

public class PoolManager {

	private final transient PgPoolOptions options;
	private transient PgPool pool;
	private transient long lastUsed;
	
	public PoolManager(PgPoolOptions options) {
		this.options = options;
		
	}
		
	public PgPool pool() {
		if (null==pool) {			
			pool = PgClient.pool(options);			
		}
		lastUsed = System.nanoTime();
		return pool;
	}
	
	public boolean clean() {
		//close pool if it has not been used for a while
		//this gives back some memory and thread resources
		if (null!=pool) {
			long duration = System.nanoTime()-lastUsed;
			if (duration > 40_000_000_000L) {//40 seconds			
				pool.close();
				
				pool = null;	
				return true;
			}
			//System.out.println("clean :"+duration);
		}
		return false;
	}
	
}
