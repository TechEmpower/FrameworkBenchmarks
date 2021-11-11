package hello.controller.persistence;

import java.util.List;

import com.mongodb.MongoClient;

import hello.domain.World;

public class WorldsMongodbRepository extends MongodbRepository<World> {
	@SuppressWarnings("unchecked")
	public WorldsMongodbRepository(MongoClient mongo, String dbName) {
		//TODO from zloster
		// This is a nice example for one kind of problem with the
		// object-oriented approach. You have a piece of functionality which is
		// not separated in an object/function. But you have to modify it. The
		// solution is to rewrite and unwind the whole thing. But this requires time, effort and devotion.
		// So copy-pasty approach was chosen.
		super(mongo, dbName, true, World.class);
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