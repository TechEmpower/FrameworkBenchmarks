package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.pronghorn.network.*;

public class GreenLightning {

	public static void main(String[] args) {
	    ServerSocketWriterStage.showWrites= true;
		GreenRuntime.run(new FrameworkTest(),args);
	}
	
}
