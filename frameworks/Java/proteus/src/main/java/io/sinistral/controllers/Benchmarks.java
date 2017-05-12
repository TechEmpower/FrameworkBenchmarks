/**
 * 
 */
package io.sinistral.controllers;

import static io.sinistral.proteus.server.ServerResponse.response;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import com.google.common.collect.ImmutableMap;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.jsoniter.output.JsonStream;

import io.sinistral.models.Fortune;
import io.sinistral.models.World;
import io.sinistral.services.MySqlService;
import io.sinistral.services.PostgresService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;

/**
 * Much of this borrowed with reverence from Light-Java
 * 
 * @author jbauer
 *
 */

@Api(tags="benchmark")
@Path("")
@Produces((MediaType.APPLICATION_JSON)) 
@Consumes((MediaType.MEDIA_TYPE_WILDCARD)) 
@Singleton
public class Benchmarks
{
	private static final String HTML_UTF8_TYPE = io.sinistral.proteus.server.MediaType.TEXT_HTML_UTF8.toString();
	private static final String PLAINTEXT_UTF8_TYPE = io.sinistral.proteus.server.MediaType.TEXT_PLAIN_UTF8.toString();
	private static final String PLAINTEXT_TYPE = io.sinistral.proteus.server.MediaType.TEXT_PLAIN.toString();
	
	@Inject 
	protected MySqlService sqlService;
	
	@Inject 
	protected PostgresService postgresService;
	
   
    public static int randomWorld() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

 
	
	
	@GET
	@Path("/db/postgres")
	@ApiOperation(value = "World postgres db endpoint",   httpMethod = "GET" , response = World.class)
	public void dbPostgres(HttpServerExchange exchange)
	{ 		
		final World world;
		
		try (final Connection connection = postgresService.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "SELECT id,randomNumber FROM world WHERE id = ?",
                    ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
                statement.setInt(1, randomWorld());
                try (ResultSet resultSet = statement.executeQuery()) {
                    resultSet.next();
                    world =  new World(
                            resultSet.getInt("id"),
                            resultSet.getInt("randomNumber"));
                }
            }
            
    		response( JsonStream.serializeToBytes(world) ).applicationJson().send(exchange);

        } catch (Exception e) {
            throw new IllegalArgumentException();
        }
		  
 		 
	}
	
	@GET
	@Path("/db/mysql")
	@ApiOperation(value = "World mysql db endpoint",   httpMethod = "GET" , response = World.class)
	public void dbMySql(HttpServerExchange exchange)
	{ 		
		final World world;
		
		try (final Connection connection = sqlService.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "SELECT id,randomNumber FROM world WHERE id = ?",
                    ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
                statement.setInt(1, randomWorld());
                try (ResultSet resultSet = statement.executeQuery()) {
                    resultSet.next();
                    world =  new World(
                            resultSet.getInt("id"),
                            resultSet.getInt("randomNumber"));
                }
            }
            
    		response( JsonStream.serializeToBytes(world) ).applicationJson().send(exchange);

        } catch (Exception e) {
            throw new IllegalArgumentException();
        }
		  
 		 
	}
	
	

	@GET
	@Path("/fortunes/mysql")
	@Produces(MediaType.TEXT_HTML)
	@ApiOperation(value = "Fortunes mysql endpoint",   httpMethod = "GET"  )
	public void fortunesMysql(HttpServerExchange exchange, HttpHandler handler ) throws Exception
	{ 
 
 		final List<Fortune> fortunes = new ArrayList<>();
 		 
 		 try (Connection connection = sqlService.getConnection();
 	             PreparedStatement statement = connection.prepareStatement(
 	                     "SELECT * FROM fortune",
 	                     ResultSet.TYPE_FORWARD_ONLY,
 	                     ResultSet.CONCUR_READ_ONLY);
 	             ResultSet resultSet = statement.executeQuery()) {
 	                while (resultSet.next()) {
 	                fortunes.add(new Fortune(
 	                        resultSet.getInt("id"),
 	                        resultSet.getString("message")));
 	            }
 	        }
 
 		 fortunes.add(new Fortune(0, "Additional fortune added at request time."));
         Collections.sort(fortunes); 
		 
		 response( views.Fortunes.template(fortunes).render().toString() ).contentType(HTML_UTF8_TYPE).send(exchange);
		  
	}
	

	@GET
	@Path("/fortunes/postgres")
	@Produces(MediaType.TEXT_HTML)
	@ApiOperation(value = "Fortunes postgres endpoint",   httpMethod = "GET"  )
	public void fortunesPostgres(HttpServerExchange exchange) throws Exception
	{ 
		

 		final List<Fortune> fortunes = new ArrayList<>();
 		 
 		  try (Connection connection = postgresService.getConnection();
 	             PreparedStatement statement = connection.prepareStatement(
 	                     "SELECT * FROM Fortune",
 	                     ResultSet.TYPE_FORWARD_ONLY,
 	                     ResultSet.CONCUR_READ_ONLY);
 	             ResultSet resultSet = statement.executeQuery()) {
 	            while (resultSet.next()) {
 	                fortunes.add(new Fortune(
 	                        resultSet.getInt("id"),
 	                        resultSet.getString("message")));
 	            }
 	        }
 
 		 fortunes.add(new Fortune(0, "Additional fortune added at request time."));
         Collections.sort(fortunes); 
		 
		 response( views.Fortunes.template(fortunes).render().toString()).contentType(HTML_UTF8_TYPE).send(exchange);
		  
	}

	
	@GET
	@Path("/plaintext")
	@Produces((MediaType.TEXT_PLAIN)) 
	@ApiOperation(value = "Plaintext endpoint",   httpMethod = "GET" )
	public void plaintext(HttpServerExchange exchange)
	{ 
		response("Hello, World!").contentType(PLAINTEXT_TYPE).send(exchange);

	}
	
	
	@GET
	@Path("/json")
	@ApiOperation(value = "Json serialization endpoint",   httpMethod = "GET" )
	public void json(HttpServerExchange exchange)
	{ 
		response( JsonStream.serializeToBytes(ImmutableMap.of("message", "Hello, World!")) ).applicationJson().send(exchange);
	}
}
