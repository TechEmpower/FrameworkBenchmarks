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
		.setSocketOption(org.xnio.Options.BACKLOG,  50000)
		.setSocketOption(org.xnio.Options.TCP_NODELAY, true)
		 .setSocketOption(org.xnio.Options.KEEP_ALIVE, false)
		.setSocketOption(org.xnio.Options.REUSE_ADDRESSES, true)
		.setServerOption(UndertowOptions.ALWAYS_SET_KEEP_ALIVE, false)
	//	.setServerOption(UndertowOptions.NO_REQUEST_TIMEOUT, 2 * 1000)

		.setServerOption(UndertowOptions.RECORD_REQUEST_START_TIME, false)
		.setServerOption(UndertowOptions.ENABLE_CONNECTOR_STATISTICS, false)
		.setServerOption(UndertowOptions.MAX_ENTITY_SIZE, config.getBytes("undertow.server.maxEntitySize"))
		.setServerOption(UndertowOptions.BUFFER_PIPELINED_DATA, true)
		.setDirectBuffers(true)
		.setBufferSize(16 * 1024)
		.setIoThreads( Runtime.getRuntime().availableProcessors()  )
		.setWorkerThreads((Runtime.getRuntime().availableProcessors() * 32))
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
