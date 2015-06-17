package controllers;

import interfaces.Constants;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import mangoo.io.routing.Response;
import models.Fortune;
import models.Message;
import models.World;
import services.DataService;
import utils.RandomUtils;

import com.google.inject.Inject;
import com.google.inject.Singleton;

@Singleton
public class ApplicationController {
	
	@Inject
	private DataService dataService;
	
	public Response index() {
		return Response.withOk().andEmptyBody();
	}

	public Response json() {
    	return Response.withOk().andJsonBody(new Message(Constants.HELLO_WORLD));
    }
	
	public Response db() {
		World world = dataService.findById(RandomUtils.getRandomId()); 
    	return Response.withOk().andJsonBody(world);
    }
	
	public Response queries(String queries) {
		List<World> worlds = dataService.getWorlds(queries);
		return Response.withOk().andJsonBody(worlds);
    }
	
	public Response plaintext() {
    	return Response.withOk().andTextBody(Constants.HELLO_WORLD);
    }
	
	public Response fortunes() {
		List<Fortune> fortunes = dataService.findAllFortunes();
		fortunes.add(new Fortune(0, Constants.FORTUNE_MESSAGE));
		Collections.sort(fortunes);
		
    	return Response.withOk().andContent("fortunes", fortunes);
    }
	
	public Response updates(String queries) {
		List<World> worldUpdates = new ArrayList<World>();
		
		List<World> worlds = dataService.getWorlds(queries);
		for (World world : worlds) {
			int randomNumber = RandomUtils.getRandomId();
			world.setRandomnumber(randomNumber);
			dataService.save(world);
			
			worldUpdates.add(world);
		}
		
		return Response.withOk().andJsonBody(worldUpdates);
    }
}