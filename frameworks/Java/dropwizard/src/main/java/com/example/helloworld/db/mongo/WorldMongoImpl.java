package com.example.helloworld.db.mongo;

import org.mongojack.DBProjection;
import org.mongojack.DBUpdate;
import org.mongojack.JacksonDBCollection;

import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.World;
import com.example.helloworld.resources.Helper;

public class WorldMongoImpl implements WorldDAO {

    private final JacksonDBCollection<World, Integer> worldCollection;

    public WorldMongoImpl(JacksonDBCollection<World, Integer> worlds) {
        worldCollection = worlds;
    }

    @Override
    public World findById(int id) {
        return worldCollection.findOneById(id, DBProjection.include("_id", "randomNumber"));
    }

    @Override
	public World[] findById(int[] ids) {
    	World[] worlds = new World[ids.length];
		for(int i = 0; i < ids.length; i++) {
			worlds[i] = findById(ids[i]);
		}
		return worlds;
	}
    
    @Override
    public World findAndModify(int worldId, int newRandomNumber) {
        World theOne = findById(worldId);
        theOne.setRandomNumber(newRandomNumber);
        worldCollection.updateById(theOne.getId(), DBUpdate.set("randomNumber", theOne.getRandomNumber()));
        return theOne;

    }

    @Override
    public World[] updatesQueries(int totalQueries) {
        final World[] worlds = new World[totalQueries];
        for (int i = 0; i < totalQueries; i++) {
            worlds[i] = findAndModify(Helper.randomWorld(), Helper.randomWorld());
        }
        return worlds;
    }

}
