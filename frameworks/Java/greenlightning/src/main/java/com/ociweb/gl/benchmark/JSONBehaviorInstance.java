package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.gl.api.HTTPRequestReader;
import com.ociweb.gl.api.HTTPResponseService;
import com.ociweb.gl.api.RestListener;
import com.ociweb.json.encode.JSONRenderer;
import com.ociweb.pronghorn.network.config.HTTPContentTypeDefaults;

public class JSONBehaviorInstance implements RestListener {


	private static final JSONRenderer<ResultObject> renderJSON = new JSONRenderer<ResultObject>()
			.startObject()
				.string("message", (o,t) -> t.write(o.payload) )
			.endObject();

	private final HTTPResponseService responseService;

	
	public JSONBehaviorInstance(GreenRuntime runtime) {
		responseService = runtime.newCommandChannel().newHTTPResponseService(1<<17);		
	}


	@Override
	public boolean restRequest(HTTPRequestReader request) {
	
		ResultObject result = new ResultObject(FrameworkTest.payload);
		
		return responseService.publishHTTPResponse(request, 
				                            HTTPContentTypeDefaults.JSON,
				                            w -> renderJSON.render(w,result)
				                            );
		
	}

}
