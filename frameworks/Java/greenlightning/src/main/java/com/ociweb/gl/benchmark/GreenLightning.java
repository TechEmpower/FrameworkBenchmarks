package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;

public class GreenLightning {

	public static void main(String[] args) {
		
		//System.setProperty("pronghorn.processors", "28"); //this is what server will see in production test
		//less stack per thread??
		GreenRuntime.run(new FrameworkTest(),args);
	
	}
	
}
