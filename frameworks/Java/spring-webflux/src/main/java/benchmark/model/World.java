package benchmark.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Document
public final class World {

    @Id
    public int id;
    public int randomnumber;

    public World(int id, int randomnumber) {
        this.id = id;
        this.randomnumber = randomnumber;
    }
}
