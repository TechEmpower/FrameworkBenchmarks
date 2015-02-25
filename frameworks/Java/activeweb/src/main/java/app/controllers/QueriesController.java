/*
Copyright 2009-2015 Igor Polevoy

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
package app.controllers;

import app.models.World;
import java.io.IOException;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Igor Polevoy: 12/18/13 4:36 PM
 * @author Eric Nielsen
 */
public class QueriesController extends DbController {
    @Override public void index() {
        view("worlds", getWorlds());
        render("/queries/index").contentType("application/json");
    }

    @Override public void jackson() throws IOException {
        JsonController.WRITER.writeValue(outputStream("application/json"), getWorlds());
    }

    @Override protected String getLayout() {
        return null;
    }

    protected List<World> getWorlds() {
        int number = getQueries();
        List<World> worlds = new ArrayList<>(number);
        for (int i = 0; i < number; i++) {
            worlds.add(World.<World>findById(randomNumber()));
        }
        return worlds;
    }

    protected int getQueries() {
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
}
