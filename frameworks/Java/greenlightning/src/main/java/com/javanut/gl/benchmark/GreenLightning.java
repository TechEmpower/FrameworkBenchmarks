package com.javanut.gl.benchmark;

import com.javanut.gl.api.GreenRuntime;
import com.javanut.pronghorn.stage.scheduling.GraphManager;

public class GreenLightning {

	public static void main(String[] args) {
			
		GraphManager.showThreadIdOnTelemetry = true;		
		GraphManager.showScheduledRateOnTelemetry = true;
		
		GreenRuntime.run(new FrameworkTest(),args);
	
	}
	
}
