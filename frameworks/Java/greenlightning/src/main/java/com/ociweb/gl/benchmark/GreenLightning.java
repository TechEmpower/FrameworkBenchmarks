package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;

public class GreenLightning {

	public static void main(String[] args) {
		
		//PipeConfig.showConfigsCreatedLargerThan = 1<<23;
		
		//System.setProperty("pronghorn.processors", "28"); //TODO: could also lower memory usage by shrinking the stack space...
		GreenRuntime.run(new FrameworkTest(),args);
	
	}
	
}
