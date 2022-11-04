package hello.repository;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.stereotype.Repository;

import hello.model.Fortune;
import hello.model.World;

@Repository
@Profile("mongo")
public class MongoDbRepository implements DbRepository {
	private final Logger log = LoggerFactory.getLogger(getClass());
	private final MongoTemplate mongoTemplate;

	public MongoDbRepository(MongoTemplate mongoTemplate) {
		this.mongoTemplate = mongoTemplate;
	}

	@Override
	public World getWorld(int id) {
		log.debug("getWorld({})", id);
		return mongoTemplate.findById(id, World.class);
	}

	@Override
	public World updateWorld(World world, int randomNumber) {
		world.randomnumber = randomNumber;
		return mongoTemplate.save(world);
	}

	@Override
	public List<Fortune> fortunes() {
		return mongoTemplate.findAll(Fortune.class);
	}
}
