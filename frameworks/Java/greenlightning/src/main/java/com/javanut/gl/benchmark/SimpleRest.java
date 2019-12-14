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
	private final JSONRenderer<RequestObject> renderJSON;
		
	public SimpleRest(GreenRuntime runtime, int maxResponseCount, int maxResponseSize, byte[] payload) {
		this.payload = payload;
		this.responseService = runtime
				.newCommandChannel()
				.newHTTPResponseService(maxResponseCount, maxResponseSize);	
		
		this.renderJSON = new JSONRenderer<RequestObject>()
				.startObject()
				.string(messageBytes, (o,t) -> t.write(payload) )
				.endObject();
		
	}
	

	
	public boolean jsonRestRequest(HTTPRequestReader request) {
	
		//this check is to postpone the work if the network has become saturated
		if (responseService.hasRoomFor(1)) {

			return responseService.publishHTTPResponse(request, 
					                            HTTPContentTypeDefaults.JSON,
					                            w -> renderJSON.render(w,new RequestObject(request))
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
