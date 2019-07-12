package com.javanut.gl.benchmark;

import com.javanut.gl.api.GreenRuntime;
import com.javanut.gl.api.HTTPRequestReader;
import com.javanut.gl.api.HTTPResponseService;
import com.javanut.gl.api.RestMethodListener;
import com.javanut.json.encode.JSONRenderer;
import com.javanut.pronghorn.network.config.HTTPContentTypeDefaults;

public class SimpleRest implements RestMethodListener {


	private final byte[] messageBytes = "message".getBytes();
	private final byte[] payload;
	private final HTTPResponseService responseService;
	
	public SimpleRest(GreenRuntime runtime, int maxResponseCount, int maxResponseSize, byte[] payload) {
		this.payload = payload;
		this.responseService = runtime
				.newCommandChannel()
				.newHTTPResponseService(maxResponseCount, maxResponseSize);		
	}
	

//	JSONRenderer<HTTPRequestReader> renderJSON = new JSONRenderer<HTTPRequestReader>()
//			.startObject()
//			.string(messageBytes, (o,t) -> t.write(FrameworkTest.payload) )
//			.endObject();
	
	public boolean jsonRestRequest(HTTPRequestReader request) {
	
		//this check is to postpone the work if the network has become saturated
		if (responseService.hasRoomFor(1)) {
			//NOTE: this is only done here for the framework test
			//      in a normal production deployment this JSONRender will only
			//      be created once and held as a member.
			JSONRenderer<HTTPRequestReader> renderJSON = new JSONRenderer<HTTPRequestReader>()
					.startObject()
					.string(messageBytes, (o,t) -> t.write(payload) )
					.endObject();
			
			return responseService.publishHTTPResponse(request, 
					                            HTTPContentTypeDefaults.JSON,
					                            w -> renderJSON.render(w,request)
					                            );
		} else {
			return false;
		}
	}
	
	
	public boolean plainRestRequest(HTTPRequestReader request) {
	
		return responseService.publishHTTPResponse(request, 	
					HTTPContentTypeDefaults.PLAIN,
					w -> w.write(payload)
				);
		
	}

}
