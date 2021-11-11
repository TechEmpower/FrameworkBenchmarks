package com.javanut.gl.benchmark;

import com.javanut.gl.api.HTTPRequestReader;
import com.javanut.pronghorn.pipe.StructuredReader;

public class RequestObject {

	public RequestObject(HTTPRequestReader request) {
		StructuredReader reader = request.structured();
		
	}

}
