package benchmark.repository;

import java.util.List;

import benchmark.model.Fortune;
import benchmark.model.World;

import org.springframework.context.annotation.Profile;
import org.springframework.data.mongodb.core.ReactiveMongoOperations;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import static org.springframework.data.mongodb.core.query.Criteria.where;
import static org.springframework.data.mongodb.core.query.Query.query;
import static org.springframework.data.mongodb.core.query.Update.update;

@Component
@Profile("mongo")
public class MongoDbRepository implements DbRepository {

    private final ReactiveMongoOperations operations;

    public MongoDbRepository(ReactiveMongoOperations operations) {
        this.operations = operations;
    }

    @Override
    public Mono<World> getWorld(int id) {
        return operations.findById(id, World.class);
    }

    @Override
    public Mono<Void> updateWorlds(List<World> worlds) {
        return Flux.fromIterable(worlds).flatMap(world -> operations.findAndModify(
                query(where("id").is(world.id)),
                update("randomNumber", world.randomnumber),
                World.class)).then();
    }

    @Override
    public Flux<Fortune> fortunes() {
        return operations.findAll(Fortune.class);
    }
}