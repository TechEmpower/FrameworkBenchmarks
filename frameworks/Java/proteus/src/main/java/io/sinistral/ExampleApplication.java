package io.sinistral;

import static io.undertow.util.Headers.CONTENT_TYPE;

import java.nio.ByteBuffer;

import com.jsoniter.output.EncodingMode;
import com.jsoniter.output.JsonStream;
import com.mysql.jdbc.log.Log;

import io.sinistral.controllers.Benchmarks;
import io.sinistral.models.Message;
import io.sinistral.proteus.ProteusApplication;
import io.sinistral.proteus.controllers.handlers.BenchmarksRouteSupplier;
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


public class ExampleApplication extends ProteusApplication
{
 
    static {
   
    	
    	JsonStream.setMode(EncodingMode.STATIC_MODE);
    	 
    	 
    }
    
    @Override
    public void buildServer()
    {
    	int httpPort = config.getInt("application.ports.http");
		
		if(System.getProperty("http.port") != null)
		{
			httpPort = Integer.parseInt(System.getProperty("http.port"));
		}
		
		System.out.println("httpPort: " + httpPort);
		
		this.ports.add(httpPort);
		
		Benchmarks controller = this.getInjector().getInstance(Benchmarks.class);
		
		HttpHandler pathsHandler = new BenchmarksRouteSupplier(controller, null).get();
		
		HttpHandler rootHandler = new SetHeaderHandler(pathsHandler, "Server", config.getString("globalHeaders.Server"));
		
		Undertow.Builder undertowBuilder = Undertow.builder().addHttpListener(httpPort, config.getString("application.host"))
				.setBufferSize(16 * 1024)
				.setIoThreads(Runtime.getRuntime().availableProcessors() * 2)
//				.setServerOption(UndertowOptions.ALWAYS_SET_DATE, true)
				.setSocketOption(org.xnio.Options.BACKLOG, 10000)
				.setServerOption(UndertowOptions.ALWAYS_SET_KEEP_ALIVE, false)
				.setServerOption(UndertowOptions.RECORD_REQUEST_START_TIME, false)
                .setServerOption(UndertowOptions.ENABLE_CONNECTOR_STATISTICS, false)
				.setServerOption(UndertowOptions.MAX_ENTITY_SIZE, config.getBytes("undertow.server.maxEntitySize"))
				.setWorkerThreads(200)
				.setHandler(rootHandler);
		
		this.undertow = undertowBuilder.build();
 		
		log.debug("Completed server build!");

    }
    
    
    public static void main( String[] args )
    {
        
        ExampleApplication app = new ExampleApplication();
        
        app.addService(SwaggerService.class);
		 
		app.addService(AssetsService.class);

		app.addService(MySqlService.class);

		app.addService(PostgresService.class);
// 
//		app.addController(Benchmarks.class);  
		
		app.start();
		
		 
		
	 
    }
    
}
