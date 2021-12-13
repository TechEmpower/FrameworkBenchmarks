package com.javanut.gl.benchmark;

import com.javanut.gl.api.GreenRuntime;
import com.javanut.gl.api.HTTPRequestReader;
import com.javanut.gl.api.HTTPResponseService;
import com.javanut.gl.api.PubSubMethodListener;
import com.javanut.gl.api.RestMethodListener;
import com.javanut.gl.api.TickListener;

import io.vertx.pgclient.PgConnectOptions;
import io.vertx.sqlclient.PoolOptions;

public class DBRest implements RestMethodListener, PubSubMethodListener, TickListener {

	private final ProcessUpdate processUpdate;
	private final ProcessFortune processFortune;
	private final ProcessQuery processQuery;
	private static transient PoolManager pm;
	
	public DBRest(GreenRuntime runtime, PgConnectOptions options, PoolOptions poolOptions, int pipelineBits, 
			      int maxResponseCount, int maxResponseSize) {
		
		pm = new PoolManager(options, poolOptions);
			
		HTTPResponseService service = runtime.newCommandChannel().newHTTPResponseService(
				                maxResponseCount, 
				                maxResponseSize);
		
		processUpdate = new ProcessUpdate(pipelineBits, service, pm);		
		processFortune = new ProcessFortune(pipelineBits, service, pm);
		processQuery = new ProcessQuery(pipelineBits, service, pm);
		
	}
	
	@Override
	public void tickEvent() { 
		
		processUpdate.tickEvent();
		processFortune.tickEvent();
		processQuery.tickEvent();
		
	}
	
	public boolean restFortuneRequest(HTTPRequestReader request) {		
		return processFortune.restFortuneRequest(request);
	}
	
	public boolean updateRestRequest(HTTPRequestReader request) {
		return processUpdate.updateRestRequest(request);
	}
	
	public boolean multiRestRequest(HTTPRequestReader request) {
		return processQuery.multiRestRequest(request);
	}

	public boolean singleRestRequest(HTTPRequestReader request) {
		return processQuery.singleRestRequest(request);
	}
	
	
}

