package benchmark.repository;

import benchmark.model.Fortune;
import benchmark.model.World;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import com.mongodb.client.model.UpdateOneModel;
import org.bson.Document;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static com.mongodb.client.model.Filters.eq;

public class MongoDbService implements DbService {

    @Override
    public List<World> getWorld(int num) {

        List<World> worldList = new ArrayList<>();
        MongoDatabase db = MongoDBFactory.INSTANCE.getDatabase();
        MongoCollection<Document> collection = db.getCollection("world");

        for (int randomId : getRandomNumberSet(num)) {
            Document doc = collection.find(eq("_id", randomId)).first();
            if (doc != null) {
                World world = new World();
                world.setId(((Double) doc.get("_id")).intValue());
                world.setRandomNumber(((Double) doc.get("randomNumber")).intValue());
                worldList.add(world);
            }
        }

        return worldList;
    }

    @Override
    public List<Fortune> getFortune() {

        List<Fortune> fortuneList = new ArrayList<>();
        MongoDatabase db = MongoDBFactory.INSTANCE.getDatabase();
        MongoCollection<Document> collection = db.getCollection("fortune");

        collection.find().forEach(doc -> {
                Fortune fortune = new Fortune();
                fortune.setId(((Double)doc.get("_id")).intValue());
                fortune.setMessage((String)doc.get("message"));
                fortuneList.add(fortune);
            });

        fortuneList.add(new Fortune(defaultFortuneId, defaultFortuneMessage));

        fortuneList.sort(Comparator.comparing(Fortune::getMessage));
        return fortuneList;
    }

    @Override
    public List<World> updateWorld(int num) {

        MongoDatabase db = MongoDBFactory.INSTANCE.getDatabase();
        MongoCollection<Document> collection = db.getCollection("world");
        List<UpdateOneModel<Document>> bulkList = new ArrayList<>();
        List<World> worldList = getWorld(num);

        for (World world : worldList) {
            int newRandomNum;
            do {
                newRandomNum = getRandomNumber();
            } while (newRandomNum == world.getRandomNumber());
            world.setRandomNumber(newRandomNum);
            bulkList.add(new UpdateOneModel<>(new Document("_id", world.getId()),
                    new Document("$set", new Document("randomNumber",
                            ((Integer)world.getRandomNumber()).doubleValue()))));
        }
        collection.bulkWrite(bulkList);

        return worldList;
    }
}
