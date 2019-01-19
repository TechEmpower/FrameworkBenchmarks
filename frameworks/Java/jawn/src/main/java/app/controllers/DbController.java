package app.controllers;

import com.google.inject.Inject;

import app.db.DbManager;
import app.helpers.Helper;
import net.javapla.jawn.core.Controller;
import net.javapla.jawn.core.Param;

public class DbController extends Controller {

    @Inject
    private DbManager db;
    
    // /db
    public void index() {
        respond().json(db.getWorld(Helper.getRandomNumber())).addHeader("Server", "jawn");
    }
    
    // /queries?queries=
    public void getQueries() {
        int param = parseQueryParam();

        respond().json(db.getWorlds(param)).addHeader("Server", "jawn");
    }
    
    // /updates?queries=
    public void getUpdates() {
        int param = parseQueryParam();

        respond().json(db.getAndUpdateWorlds(param)).addHeader("Server", "jawn");
    }
    
    private int parseQueryParam() {
        Param queries = param("queries");
        Integer param = queries.asInt(1);
        if (param > 500) param = 500;
        else if (param < 1) param = 1;
        
        return param;
    }
}
