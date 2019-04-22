package hello.controller.persistence;

import java.util.List;

import com.mongodb.MongoClient;
import com.strategicgains.repoexpress.mongodb.MongodbRepository;

import hello.domain.World;

public class WorldsMongodbRepository extends MongodbRepository<World> {
	@SuppressWarnings("unchecked")
	public WorldsMongodbRepository(MongoClient mongo, String dbName) {
		super(mongo, dbName, World.class);
	}

	public World find(int id) {
		return getDataStore().find(World.class, "id", (long) id).retrievedFields(false, "_id")
				.get();
	}

	public World findAll(List<Integer> ids) {
		return getDataStore().find(World.class).field("id").in(ids).retrievedFields(false, "_id")
				.get();
	}
}