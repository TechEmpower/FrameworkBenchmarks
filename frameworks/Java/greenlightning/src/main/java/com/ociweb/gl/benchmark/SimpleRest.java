package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.gl.api.HTTPRequestReader;
import com.ociweb.gl.api.HTTPResponseService;
import com.ociweb.gl.api.RestMethodListener;
import com.ociweb.json.encode.JSONRenderer;
import com.ociweb.pronghorn.network.config.HTTPContentTypeDefaults;

public class SimpleRest implements RestMethodListener {


	private final HTTPResponseService responseService;
	
	public SimpleRest(GreenRuntime runtime, int maxResponseCount, int maxResponseSize) {
		responseService = runtime.newCommandChannel().newHTTPResponseService(maxResponseCount, maxResponseSize);		
	}
	
	public boolean jsonRestRequest(HTTPRequestReader request) {
	
		//this check is to postpone the work if the network has become saturated
		if (responseService.hasRoomFor(1)) {
			//NOTE: this is only done here for the framework test
			//      in a normal production deployment this JSONRender will only
			//      be created once and held as a member.
			JSONRenderer<HTTPRequestReader> renderJSON = new JSONRenderer<HTTPRequestReader>()
					.startObject()
					.string("message", (o,t) -> t.write(FrameworkTest.payload) )
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
					w -> w.write(FrameworkTest.payload)
				);
		
	}

}
