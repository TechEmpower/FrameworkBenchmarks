package io.sinistral;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;

import io.sinistral.controllers.Benchmarks;
import io.sinistral.proteus.ProteusApplication;
import io.sinistral.proteus.services.AssetsService;
import io.sinistral.proteus.services.SwaggerService;
import io.sinistral.services.MySqlService;
import io.sinistral.services.PostgresService;

/**
 * Hello world!
 *
 */
public class ExampleApplication extends ProteusApplication
{
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
