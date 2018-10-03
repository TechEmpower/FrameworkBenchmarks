package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.gl.api.HTTPRequestReader;
import com.ociweb.gl.api.HTTPResponseService;
import com.ociweb.gl.api.RestListener;
import com.ociweb.json.encode.JSONRenderer;
import com.ociweb.pronghorn.network.config.HTTPContentTypeDefaults;

public class JSONBehaviorInstance implements RestListener {

	byte[] messagePayload;

	private static final JSONRenderer<JSONBehaviorInstance> renderJSON = new JSONRenderer<JSONBehaviorInstance>()
			.startObject()
				.string("message", (o,t) -> t.write(o.messagePayload) )
			.endObject();

	private final HTTPResponseService responseService;

	
	public JSONBehaviorInstance(GreenRuntime runtime) {
		responseService = runtime.newCommandChannel().newHTTPResponseService(1<<16);		
	}


	@Override
	public boolean restRequest(HTTPRequestReader request) {
	
		messagePayload = FrameworkTest.payload;
		
		return responseService.publishHTTPResponse(request, 
				                            HTTPContentTypeDefaults.JSON,
				                            w -> renderJSON.render(w,this)
				                            );
		
	}

}
