package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.gl.api.HTTPRequestReader;
import com.ociweb.gl.api.HTTPResponseService;
import com.ociweb.gl.api.RestListener;
import com.ociweb.gl.api.Writable;
import com.ociweb.pronghorn.network.config.HTTPContentTypeDefaults;
import com.ociweb.pronghorn.pipe.ChannelWriter;

public class PlainBehaviorInstance implements RestListener {

	private final HTTPResponseService plainResponseService;
	
	//a lambda could be used if you like
	private Writable writePayload = new Writable() {
		@Override
		public void write(ChannelWriter writer) {
			writer.write(FrameworkTest.payload);
		}		
	};
	
	public PlainBehaviorInstance(GreenRuntime runtime) {
		plainResponseService = runtime.newCommandChannel().newHTTPResponseService(1<<14, 1<<8); 
	}

	@Override
	public boolean restRequest(HTTPRequestReader request) {
		
		return plainResponseService.publishHTTPResponse(request, 	
					HTTPContentTypeDefaults.PLAIN,
					writePayload
				);
		
	}

}
