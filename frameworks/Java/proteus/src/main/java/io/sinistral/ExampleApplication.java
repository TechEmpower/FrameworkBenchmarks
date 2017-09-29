package io.sinistral;

import static io.undertow.util.Headers.CONTENT_TYPE;

import java.nio.ByteBuffer;

import com.jsoniter.output.EncodingMode;
import com.jsoniter.output.JsonStream;

import io.sinistral.controllers.Benchmarks;
import io.sinistral.models.Message;
import io.sinistral.proteus.ProteusApplication;
import io.sinistral.proteus.services.AssetsService;
import io.sinistral.proteus.services.SwaggerService;
import io.sinistral.services.MySqlService;
import io.sinistral.services.PostgresService;
import io.undertow.Handlers;
import io.undertow.Undertow;
import io.undertow.UndertowOptions;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.server.handlers.PathHandler;
import io.undertow.server.handlers.SetHeaderHandler;
import io.undertow.util.Headers;

/**
 * Hello world!
 *
 */
public class ExampleApplication extends ProteusApplication
{
 
    static {
   
    	
    	JsonStream.setMode(EncodingMode.STATIC_MODE);
    	 
    	 
    }
    
    
    public static void main( String[] args )
    {
        
        ExampleApplication app = new ExampleApplication();
        
        app.addService(SwaggerService.class);
		 
		app.addService(AssetsService.class);

		app.addService(MySqlService.class);

		app.addService(PostgresService.class);
 
		app.addController(Benchmarks.class);  
		
		app.start();
		
		 
		
	 
    }
    
}
