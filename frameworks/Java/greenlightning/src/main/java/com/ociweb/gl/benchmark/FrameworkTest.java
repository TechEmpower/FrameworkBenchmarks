package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenApp;
/**
 * ************************************************************************
 * For greenlightning support, training or feature reqeusts please contact:
 *   info@objectcomputing.com   (314) 579-0066
 * ************************************************************************
 */
import com.ociweb.gl.api.GreenFramework;
import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.pronghorn.stage.scheduling.GraphManager;

public class FrameworkTest implements GreenApp {

	static byte[] payload = "Hello, World!".getBytes();

	private int bindPort;
    private String host;
    private int concurrentWritesPerChannel;
    private int queueLengthOfPendingRequests;
    private int telemetryPort;//for monitoring

    public FrameworkTest() {
    	//this server works best with  -XX:+UseNUMA
    	this("*.*.*.*",8080, 40, 16*1024, -1); //set -1 to 8098 or some port for remote monitoring over http
    }
    
    public FrameworkTest(String host, int port, int concurrentWritesPerChannel, int queueLengthOfPendingRequests, int telemetryPort) {
    	this.bindPort = port;
    	this.host = host;
    	this.concurrentWritesPerChannel = concurrentWritesPerChannel;
    	this.queueLengthOfPendingRequests = queueLengthOfPendingRequests;
    	this.telemetryPort = telemetryPort;
    }

	@Override
    public void declareConfiguration(GreenFramework framework) {
		
		framework.useHTTP1xServer(bindPort, this::parallelBehavior) //auto-scale of tracks based on cores
    			 .setHost(host)
    			 .setConcurrentChannelsPerDecryptUnit(concurrentWritesPerChannel)
    			 .setConcurrentChannelsPerEncryptUnit(concurrentWritesPerChannel)
    			 .setMaxQueueIn(queueLengthOfPendingRequests)
    	         .useInsecureServer(); //turn off TLS, it is on by default
        
		framework.defineRoute()
		         .path("/plaintext")
		         .routeId(Struct.PLAINTEXT_ROUTE);
		
		framework.defineRoute()
		        .path("/json")
		        .routeId(Struct.JSON_ROUTE);
		
		if (telemetryPort>0) {
			GraphManager.showThreadIdOnTelemetry = true;
			framework.enableTelemetry(telemetryPort);
		}
    }

	public void parallelBehavior(GreenRuntime runtime) {

		runtime.addRestListener("PlainResponder",new PlainBehaviorInstance(runtime))
		       .includeRoutes(Struct.PLAINTEXT_ROUTE);

		runtime.addRestListener("JSONResponder",new JSONBehaviorInstance(runtime))
		       .includeRoutes(Struct.JSON_ROUTE);

	}
	 
    @Override
    public void declareBehavior(GreenRuntime runtime) {   
    	
    }
  
}
