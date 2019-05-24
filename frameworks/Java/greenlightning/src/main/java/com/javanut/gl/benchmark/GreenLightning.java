package com.javanut.gl.benchmark;

import com.javanut.gl.api.GreenRuntime;
import com.javanut.pronghorn.stage.scheduling.GraphManager;

public class GreenLightning {

	public static void main(String[] args) {
		
//		System.setProperty("vertx.options.maxEventLoopExecuteTime","10000000000"); 
//		
//		VertxOptions options = new VertxOptions();
//    	options.setBlockedThreadCheckIntervalUnit(TimeUnit.MINUTES);
//    	options.setBlockedThreadCheckInterval(20);
    	    	
		//ServerSocketReaderStage.showRequests = true;
		
		//ScriptedNonThreadScheduler.debugStageOrder = System.out;
		
		//TODO: we have reactors in the wrong order after the consume ordering stage.. must fix
		//TODO: the TrackHTTPResponseListener private class is the new high cpu stage for full test.
		
		//   
		//     Jan 10, 2019 1:47:45 PM io.vertx.core.impl.BlockedThreadChecker
		//     WARNING: Thread Thread[vert.x-eventloop-thread-0,5,main] has been blocked for 2247 ms, time limit is 2000 ms
		
		//TODO: must check factors for pipe splits...
		
		//PipeConfig.showConfigsCreatedLargerThan = 1<<23;
		GraphManager.showThreadIdOnTelemetry = true;		
		GraphManager.showScheduledRateOnTelemetry = true;
		GraphManager.showMessageCountRangeOnTelemetry = true;

		
		//TODO: we must pre build the graph to allow for giant images
		//GraphManager.combineCommonEdges = true;
		
	//ServerSocketReaderStage.showRequests = true;
	//	ServerSocketWriterStage.showWrites = true;
		
		//System.setProperty("pronghorn.processors", "28"); //simulate the techempower testing box
		
		//test client is the same old 22 version so issue MUST be
		//in socket reader OR the http parser.
		//with 12 we ran normal 84 and found issues but these are NOT shared connections..
		//System.setProperty("pronghorn.processors", "12"); //with 6-28 we have issues...
		
		//TODO: update for 5 way MUST ensure no multiple of 5 logic is fixed...
		
		//reduce pipes for less memory used by test to reach 16K test..
		//TODO: block other 5 values..
		
		//System.setProperty("pronghorn.processors", "28"); //set lower since we do testing here... //6 , 8,  12,  16
		                           
	//	System.setProperty("pronghorn.processors", "6"); 
		System.setProperty("pronghorn.processors", "28"); 
		
		//TODO: need 3 tracks and ramp it up to 750K+ to show 7M on the server
		//TODO: fix order super...
		
		//./wrk -t12 -c512 -d12s http://127.0.0.1:8080/json
		//./wrk -t12 -c3552 -d32s -s./pipeline.lua  http://127.0.0.1:8080/plaintext -- 16

		
		//System.setProperty("pronghorn.processors", "1");
		GreenRuntime.run(new FrameworkTest(),args);
	
	}
	
}
