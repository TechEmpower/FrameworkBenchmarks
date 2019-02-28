package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.gl.api.HTTPRequestReader;
import com.ociweb.gl.api.HTTPResponseService;
import com.ociweb.gl.api.PubSubMethodListener;
import com.ociweb.gl.api.RestMethodListener;
import com.ociweb.gl.api.TickListener;

import io.reactiverse.pgclient.PgPoolOptions;

public class DBRest implements RestMethodListener, PubSubMethodListener, TickListener {

	private final ProcessUpdate processUpdate;
	private final ProcessFortune processFortune;
	private final ProcessQuery processQuery;
	private final transient PoolManager pm;
	
	public DBRest(GreenRuntime runtime, PgPoolOptions options, int pipelineBits, int maxResponseCount, int maxResponseSize) {
		
		pm = new PoolManager(options);
		
		HTTPResponseService service = runtime.newCommandChannel().newHTTPResponseService(maxResponseCount, maxResponseSize);

		processUpdate = new ProcessUpdate(pipelineBits, service, pm);
		processFortune = new ProcessFortune(pipelineBits, service, pm);
		processQuery = new ProcessQuery(pipelineBits, service, pm);
		
	}
	
	@Override
	public void tickEvent() { 
		
		processUpdate.tickEvent();
		processFortune.tickEvent();
		processQuery.tickEvent();
		//removes DB pool if it is not longer in use
		pm.clean();
			
		
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

