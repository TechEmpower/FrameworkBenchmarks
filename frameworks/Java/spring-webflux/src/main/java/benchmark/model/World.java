package benchmark.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

@Document
public final class World {

    @Id
    public int id;
    @Field("randomNumber")
    public int randomnumber;

    public World(int id, int randomnumber) {
        this.id = id;
        this.randomnumber = randomnumber;
    }
}
