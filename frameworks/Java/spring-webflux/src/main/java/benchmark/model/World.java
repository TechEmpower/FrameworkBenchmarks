package benchmark.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Document
public final class World {

    @Id
    public int id;
    public int randomNumber;

    public World(int id, int randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }
}
