package app.controllers;

import java.util.concurrent.ThreadLocalRandom;

import com.google.inject.Inject;

import app.db.DbManager;
import app.models.World;
import net.javapla.jawn.core.ApplicationController;
import net.javapla.jawn.core.Param;

public class DbController extends ApplicationController {

    private static final int NUMBER_OF_ROWS = 10_000;
    
    @Inject
    private DbManager db;
    
    public void index() {
        respond().json(db.getWorld(getRandomNumber()));
    }
    
    public void getQueries() {
        int param = parseQueryParam();
        
        World[] worlds = new World[param];
        for (int i = 0; i < param; i++) {
            worlds[i] = db.getWorld(getRandomNumber());
        }
        respond().json(worlds);
    }
    
    public void getUpdates() {
        int param = parseQueryParam();
        
        World[] worlds = new World[param];
        for (int i = 0; i < param; i++) {
            World world = db.getWorld(getRandomNumber());
            world.randomNumber = getRandomNumber();
            worlds[i] = world;
        }
        db.updateWorlds(worlds);
        respond().json(worlds);
    }
    
    private int getRandomNumber() {
        return ThreadLocalRandom.current().nextInt(NUMBER_OF_ROWS) + 1;
    }
    
    private int parseQueryParam() {
        Param queries = param("queries");
        Integer param = queries.asInt(1);
        if (param > 500) param = 500;
        else if (param < 1) param = 1;
        
        return param;
    }
}
