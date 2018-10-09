package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.gl.api.HTTPRequestReader;
import com.ociweb.gl.api.HTTPResponseService;
import com.ociweb.gl.api.RestListener;
import com.ociweb.json.encode.JSONRenderer;
import com.ociweb.pronghorn.network.config.HTTPContentTypeDefaults;

public class JSONBehaviorInstance implements RestListener {


	private final HTTPResponseService responseService;

	
	public JSONBehaviorInstance(GreenRuntime runtime) {
		responseService = runtime.newCommandChannel().newHTTPResponseService(1<<14, 1<<8);		
	}


	@Override
	public boolean restRequest(HTTPRequestReader request) {
	
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
		
	}

}
