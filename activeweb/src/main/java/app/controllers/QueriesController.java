/*
Copyright 2009-2010 Igor Polevoy 

Licensed under the Apache License, Version 2.0 (the "License"); 
you may not use this file except in compliance with the License. 
You may obtain a copy of the License at 

http://www.apache.org/licenses/LICENSE-2.0 

Unless required by applicable law or agreed to in writing, software 
distributed under the License is distributed on an "AS IS" BASIS, 
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
See the License for the specific language governing permissions and 
limitations under the License. 
*/

/**
 * @author Igor Polevoy: 12/18/13 4:36 PM
 */

package app.controllers;

import app.models.World;
import org.javalite.activeweb.AppController;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public class QueriesController extends AppController {

    public void index() {

        view("worlds", getWorlds());
        render().contentType("application/json").header("Date", new Date().toString());
    }

    protected List<World> getWorlds() {
        int number = getQueries();
        List<World> worlds = new ArrayList<>(number);
        for (int i = 0; i < number; i++) {
            worlds.add(World.<World>findById(randomNumber()));
        }
        return worlds;
    }

    public int getQueries() {
        int queries;
        try {
            queries = Integer.parseInt(param("queries"));
        } catch (Exception e) {
            queries = 1;
        }
        if (queries > 500) {
            queries = 500;
        } else if (queries < 1) {
            queries = 1;
        }
        return queries;
    }
    protected int randomNumber(){
        return ThreadLocalRandom.current().nextInt(10000) + 1;
    }

    @Override
    protected String getLayout() {
        return null;
    }
}
