package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;

public class GreenLightning {

	public static void main(String[] args) {
		//ServerSocketWriterStage.showWrites=true;
	
		//  [{"id":1480,"randomNumber":1784090351}
		//  ,{"id":6038,"randomNumber":-447995528}
		//  ,{"id":2669,"randomNumber":493033553}
		//  ,{"id":2487,"randomNumber":511963400}]
		
		GreenRuntime.run(new FrameworkTest(),args);
	
	}
	
}
