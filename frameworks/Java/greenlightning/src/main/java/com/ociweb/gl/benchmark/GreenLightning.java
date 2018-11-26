package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;

public class GreenLightning {

	public static void main(String[] args) {
		
		//PipeConfig.showConfigsCreatedLargerThan = 1<<23;
		///GraphManager.showThreadIdOnTelemetry = true;
		
		//System.setProperty("pronghorn.processors", "28"); 
		GreenRuntime.run(new FrameworkTest(),args);
	
	}
	
}
