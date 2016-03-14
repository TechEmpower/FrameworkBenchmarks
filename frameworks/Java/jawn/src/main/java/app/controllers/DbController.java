package app.controllers;

import net.javapla.jawn.core.Controller;
import net.javapla.jawn.core.Param;
import app.db.DbManager;
import app.helpers.Helper;
import app.models.World;

import com.google.inject.Inject;

public class DbController extends Controller {

    @Inject
    private DbManager db;
    
    // /db
    public void index() {
        respond().json(db.getWorld(Helper.getRandomNumber()));
    }
    
    // /queries?queries=
    public void getQueries() {
        int param = parseQueryParam();
        
        respond().json(db.getWorlds(param));
    }
    
    // /updates?queries=
    public void getUpdates() {
        int param = parseQueryParam();
        
        World[] worlds = db.getWorlds(param);
        for (int i = 0; i < param; i++) {
            worlds[i].randomNumber = Helper.getRandomNumber();
        }
        db.updateWorlds(worlds);
        respond().json(worlds);
    }
    
    private int parseQueryParam() {
        Param queries = param("queries");
        Integer param = queries.asInt(1);
        if (param > 500) param = 500;
        else if (param < 1) param = 1;
        
        return param;
    }
}
