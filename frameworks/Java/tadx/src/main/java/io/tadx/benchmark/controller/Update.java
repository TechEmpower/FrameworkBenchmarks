package io.tadx.benchmark.controller;

import io.tadx.benchmark.entity.World;
import io.tadx.data.DbStorage;
import io.tadx.web.HttpMethod;
import io.tadx.web.annotation.*;

import java.util.SplittableRandom;

/**
 * EN: The entry point of the application.
 */

@RestController
public class Update {


    private static final SplittableRandom RANDOM = new SplittableRandom();
    private final DbStorage dbStorage;

    public Update(DbStorage dbStorage) {
        this.dbStorage = dbStorage;
    }

    @RestFunction(mapping = "/update_rest", method = HttpMethod.GET)
    public World[] execute(int queries) {
        if (queries < 1) {
            queries = 1;
        } else if (queries > 500) {
            queries = 500;
        }
        World[] worlds = new World[queries];
        //ArrayList<DataMap> updates = new ArrayList<>();
        for (int i = 0; i < queries; i++) {
            worlds[i] = dbStorage.findEntity(World.class, randomWorld());
            //updates.add(DataMap.createNew().put("id", worlds[i].id).put("randomnumber", randomWorld()));
            dbStorage.execute("update world set randomnumber=? where id=?", randomWorld(), worlds[i].id);
        }
        //dbStorage.executeBatch("update world set randomnumber={randomnumber} where id={id}", updates);

        return worlds;
    }

    static int randomWorld() {
        return 1 + RANDOM.nextInt(10000);
    }

}
