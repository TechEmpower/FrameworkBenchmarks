package com.javanut.gl.benchmark;

import com.javanut.gl.api.GreenRuntime;
import com.javanut.pronghorn.stage.scheduling.GraphManager;

public class GreenLightning {

	public static void main(String[] args) {
		//System.setProperty("pronghorn.processors", "28");//only for local testing, do not send to tech empower
		
		GraphManager.showThreadIdOnTelemetry = true;		
		GraphManager.showScheduledRateOnTelemetry = true;
		GraphManager.showMessageCountRangeOnTelemetry = true;
		
		GreenRuntime.run(new FrameworkTest(),args);
	
	}
	
}
