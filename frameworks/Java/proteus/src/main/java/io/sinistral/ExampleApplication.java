package io.sinistral;

import com.jsoniter.output.EncodingMode;
import com.jsoniter.output.JsonStream;

import io.sinistral.controllers.Benchmarks;
import io.sinistral.proteus.ProteusApplication;
import io.sinistral.proteus.controllers.handlers.BenchmarksRouteSupplier;
import io.sinistral.proteus.services.AssetsService;
import io.sinistral.proteus.services.SwaggerService;
import io.sinistral.services.MySqlService;
import io.sinistral.services.PostgresService;
import io.undertow.Undertow;
import io.undertow.UndertowOptions;
import io.undertow.server.HttpHandler;
import io.undertow.server.handlers.SetHeaderHandler;

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
		
		Benchmarks controller = injector.getInstance(Benchmarks.class);
		
		HttpHandler pathsHandler = new BenchmarksRouteSupplier(controller, null).get();
		
		HttpHandler rootHandler = new SetHeaderHandler(pathsHandler, "Server", config.getString("globalHeaders.Server"));
		
		Undertow.Builder undertowBuilder = Undertow.builder().addHttpListener(httpPort, config.getString("application.host"))
				.setBufferSize(16 * 1024)
				.setDirectBuffers(true)
				.setIoThreads(Runtime.getRuntime().availableProcessors() * 2)
				.setSocketOption(org.xnio.Options.BACKLOG, 10000)
				.setServerOption(UndertowOptions.ALWAYS_SET_KEEP_ALIVE, false)
				.setServerOption(UndertowOptions.RECORD_REQUEST_START_TIME, false)
				.setServerOption(UndertowOptions.MAX_ENTITY_SIZE, config.getBytes("undertow.server.maxEntitySize"))
				.setWorkerThreads(Runtime.getRuntime().availableProcessors() * 8)
				.setHandler(rootHandler);
		
		this.undertow = undertowBuilder.build();

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
