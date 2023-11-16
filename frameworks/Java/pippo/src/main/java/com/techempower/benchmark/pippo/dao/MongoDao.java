package com.techempower.benchmark.pippo.dao;

import com.mongodb.MongoClient;
import com.mongodb.MongoClientOptions;
import com.mongodb.ServerAddress;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.MongoDatabase;
import com.mongodb.client.model.Filters;
import com.mongodb.client.model.UpdateOneModel;
import com.techempower.benchmark.pippo.BenchmarkEnvironment;
import com.techempower.benchmark.pippo.BenchmarkUtils;
import com.techempower.benchmark.pippo.model.Fortune;
import com.techempower.benchmark.pippo.model.World;
import org.bson.Document;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class MongoDao implements Dao {

	@Override
	public World getRandomWorld() {
		Document document = getWorldsCollection().find(Filters.eq("_id", BenchmarkUtils.random())).first();
		int id = document.getInteger("_id");
		int random = document.getInteger("randomNumber");
		return new World(id, random);
	}

	@Override
	public List<World> getRandomWorlds(int count) {
		List<World> worlds = new ArrayList<>();
		MongoCollection<Document> collection = getWorldsCollection();
		for (int i = 0; i < count; i++) {
			Document document = collection.find(Filters.eq("_id", BenchmarkUtils.random())).first();
			int id = document.getInteger("_id");
			int random = document.getInteger("randomNumber");
			worlds.add(new World(id, random));
		}
		return worlds;
	}

	@Override
	public void updateRandomWorlds(List<World> worlds) {
		MongoCollection<Document> collection = getWorldsCollection();
		List<UpdateOneModel<Document>> updates = new ArrayList<>();
		for (World world : worlds) {
			updates.add(
				new UpdateOneModel<>(
					new Document("_id", world.id),
					new Document("$set", new Document("randomNumber", world.randomNumber))
				)
			);
		}
		collection.bulkWrite(updates);
	}

	@Override
	public List<Fortune> getFortunes() {
		List<Fortune> fortunes = new ArrayList<>();
		try (MongoCursor<Document> cursor = getFortunesCollection().find().iterator()) {
			while (cursor.hasNext()) {
				Document document = cursor.next();
				fortunes.add(new Fortune(
					document.getInteger("_id"),
					document.getString("message")
				));
			}
			return fortunes;
		}
	}

	@Override
	public void close() {
		if (client != null)
			client.close();
	}

	private MongoClient getClient() {
		if (client == null) {
			clientLock.lock();
			try {
				if (client == null) {
					MongoClientOptions options = MongoClientOptions.builder()
													 .connectionsPerHost(
														 switch (BenchmarkEnvironment.$()) {
															 case Citrine -> 100;
															 case Azure -> 100;
															 case Unknown -> 50;
														 }
													 )
													 .build();
					client = new MongoClient(new ServerAddress("tfb-database", 27017), options);
				}
			} finally {
				clientLock.unlock();
			}
		}
		return client;
	}

	private MongoDatabase getDatabase() {
		if (database == null) {
			databaseLock.lock();
			try {
				if (database == null)
					database = getClient().getDatabase("hello_world");
			} finally {
				databaseLock.unlock();
			}
		}
		return database;
	}

	private MongoCollection<Document> getWorldsCollection() {
		if (worldsCollection == null) {
			worldsCollectionLock.lock();
			try {
				if (worldsCollection == null)
					worldsCollection = getDatabase().getCollection("world");
			} finally {
				worldsCollectionLock.unlock();
			}
		}
		return worldsCollection;
	}

	private MongoCollection<Document> getFortunesCollection() {
		if (fortunesCollection == null) {
			fortunesCollectionLock.lock();
			try {
				if (fortunesCollection == null)
					fortunesCollection = database.getCollection("fortune");
			} finally {
				fortunesCollectionLock.unlock();
			}
		}
		return fortunesCollection;
	}

	private MongoClient client = null;	// lazy init
	private final Lock clientLock = new ReentrantLock();
	private MongoDatabase database = null;	// lazy init
	private final Lock databaseLock = new ReentrantLock();
	private MongoCollection<Document> worldsCollection = null;	// lazy init
	private final Lock worldsCollectionLock = new ReentrantLock();
	private MongoCollection<Document> fortunesCollection = null;	// lazy init
	private final Lock fortunesCollectionLock = new ReentrantLock();

}