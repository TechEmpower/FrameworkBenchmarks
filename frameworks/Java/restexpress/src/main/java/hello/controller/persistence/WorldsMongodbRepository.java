package hello.controller.persistence;

import hello.domain.World;

import com.mongodb.Mongo;
import com.strategicgains.repoexpress.mongodb.MongodbRepository;

public class WorldsMongodbRepository
extends MongodbRepository<World, Long>
{
	@SuppressWarnings("unchecked")
    public WorldsMongodbRepository(Mongo mongo, String dbName)
    {
	    super(mongo, dbName, World.class);
    }

	public World find(int id)
	{
		return getDataStore().find(World.class, "id", (long) id).retrievedFields(false, "_id").get();
	}
}
