package hello.repository;

import java.util.ArrayList;
import java.util.List;

import org.springframework.context.annotation.Profile;
import org.springframework.data.mongodb.core.BulkOperations;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Repository;

import com.mongodb.bulk.BulkWriteResult;
import hello.Utils;
import hello.model.Fortune;
import hello.model.World;

@Repository
@Profile("mongo")
public class MongoDbRepository implements DbRepository {
	private final MongoTemplate mongoTemplate;

	public MongoDbRepository(MongoTemplate mongoTemplate) {
		this.mongoTemplate = mongoTemplate;
	}

	@Override
	public World getWorld(int id) {
		return mongoTemplate.findById(id, World.class);
	}

	@Override
	public void updateWorlds(List<World> worlds) {
		BulkOperations bulkOps = mongoTemplate.bulkOps(BulkOperations.BulkMode.UNORDERED, World.class);
		for (World world : worlds) {
			Query query = new Query().addCriteria(new Criteria("_id").is(world.id));
			Update update = new Update().set("randomNumber", world.randomNumber);
			bulkOps.updateOne(query, update);
		}
		bulkOps.execute();
	}

	@Override
	public List<Fortune> fortunes() {
		return mongoTemplate.findAll(Fortune.class);
	}
}
