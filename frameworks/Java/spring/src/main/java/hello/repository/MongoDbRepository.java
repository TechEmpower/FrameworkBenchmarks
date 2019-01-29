package hello.repository;

import hello.model.Fortune;
import hello.model.World;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.stereotype.Component;

import java.util.List;

import static org.springframework.data.mongodb.core.FindAndModifyOptions.options;
import static org.springframework.data.mongodb.core.query.Criteria.where;
import static org.springframework.data.mongodb.core.query.Query.query;
import static org.springframework.data.mongodb.core.query.Update.update;

@Component
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
    public World findAndUpdateWorld(int id, int randomNumber) {
        return mongoTemplate.findAndModify(
                query(where("id").is(id)),
                update("randomNumber", randomNumber),
                options().returnNew(true),
                World.class);
    }

    @Override
    public List<Fortune> fortunes() {
        return mongoTemplate.findAll(Fortune.class);
    }
}
