package com.example.helloworld.db.mongo;

import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.World;
import com.example.helloworld.resources.Helper;
import org.mongojack.DBUpdate;
import org.mongojack.DBProjection;
import org.mongojack.DBQuery;
import org.mongojack.DBUpdate;
import org.mongojack.JacksonDBCollection;

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
