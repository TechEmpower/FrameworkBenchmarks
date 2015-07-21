package mangooio.controllers;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import interfaces.Constants;
import io.undertow.util.StatusCodes;

import java.util.UUID;

import mangoo.io.core.Application;
import mangoo.io.testing.MangooRequest;
import mangoo.io.testing.MangooResponse;
import models.Fortune;
import models.World;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import services.DataService;
import utils.RandomUtils;
import de.svenkubiak.embeddedmongodb.EmbeddedMongo;

public class ApplicationControllerTest {
	private static final String HELLO_WORLD_JSON = "{\"message\":\"Hello, World!\"}";
	
	@BeforeClass()
	public static void preloadData() {
		DataService dataService = Application.getInjector().getInstance(DataService.class);
		for (int i=0; i < Constants.ROWS; i++) {
			World world = new World(i + 1, RandomUtils.getRandomId());
			dataService.save(world);
			
			Fortune fortune = new Fortune(i + 1, UUID.randomUUID().toString());
			dataService.save(fortune);
		}
	}

	@Test
	public void testIndex() {
		MangooResponse mangooResponse = MangooRequest.get("/").execute();
		
		assertNotNull(mangooResponse);
		assertEquals(StatusCodes.OK, mangooResponse.getStatusCode());
	}
	
	@Test
	public void testJson() {
		MangooResponse mangooResponse = MangooRequest.get("/json").execute();
		
		assertNotNull(mangooResponse);
		assertEquals(StatusCodes.OK, mangooResponse.getStatusCode());
		assertEquals(HELLO_WORLD_JSON, mangooResponse.getContent());
	}
	
	@Test
	public void testDb() {
		MangooResponse mangooResponse = MangooRequest.get("/db").execute();
		
		assertNotNull(mangooResponse);
		assertEquals(StatusCodes.OK, mangooResponse.getStatusCode());
		assertTrue(mangooResponse.getContent().contains("id"));
		assertTrue(mangooResponse.getContent().contains("randomNumber"));
	}
	
	@Test
	public void testQueries() {
		int queries = RandomUtils.getRandomId();
		MangooResponse mangooResponse = MangooRequest.get("/db?queries=" + queries).execute();
		
		assertNotNull(mangooResponse);
		assertEquals(StatusCodes.OK, mangooResponse.getStatusCode());
		assertTrue(mangooResponse.getContent().contains("id"));
		assertTrue(mangooResponse.getContent().contains("randomNumber"));
	}
	
	@Test
	public void testPlaintext() {
		MangooResponse mangooResponse = MangooRequest.get("/plaintext").execute();
		
		assertNotNull(mangooResponse);
		assertEquals(StatusCodes.OK, mangooResponse.getStatusCode());
		assertEquals(Constants.HELLO_WORLD, mangooResponse.getContent());
	}
	
	@Test
	public void testFortunes() {
		MangooResponse mangooResponse = MangooRequest.get("/fortunes").execute();
		
		assertNotNull(mangooResponse);
		assertEquals(StatusCodes.OK, mangooResponse.getStatusCode());
		assertTrue(mangooResponse.getContent().contains("id"));
		assertTrue(mangooResponse.getContent().contains(Constants.FORTUNE_MESSAGE));
	}
	
	@Test
	public void testUpdates() {
		int queries = RandomUtils.getRandomId();
		MangooResponse mangooResponse = MangooRequest.get("/updates?queries=" + queries).execute();
		
		assertNotNull(mangooResponse);
		assertEquals(StatusCodes.OK, mangooResponse.getStatusCode());
		assertTrue(mangooResponse.getContent().contains("id"));
	}
	
	@AfterClass
	public static void stopMongoDB() {
		EmbeddedMongo.DB.stop();
	}
}