package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.pronghorn.stage.scheduling.GraphManager;

public class GreenLightning {

	public static void main(String[] args) {

		GraphManager.showThreadIdOnTelemetry = true;
		GraphManager.showScheduledRateOnTelemetry = true;
		                      
		GreenRuntime.run(new FrameworkTest(),args);
	
	}
	
}
