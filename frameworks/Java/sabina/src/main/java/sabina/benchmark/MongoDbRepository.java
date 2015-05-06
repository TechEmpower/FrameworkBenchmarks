/*
 * Copyright © 2015 Juan José Aguililla. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */

package sabina.benchmark;

import static com.mongodb.client.model.Filters.eq;
import static java.lang.Integer.parseInt;
import static sabina.benchmark.Application.DB_ROWS;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

import com.mongodb.*;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import org.bson.Document;

final class MongoDbRepository implements Repository {
    private MongoCollection<Document> worldCollection;
    private MongoCollection<Document> fortuneCollection;

    MongoDbRepository (Properties settings) {
        final int PORT = parseInt (settings.getProperty ("mongodb.port"));
        final String HOST = settings.getProperty ("mongodb.host");
        final String DATABASE = settings.getProperty ("mongodb.database");
        final String WORLD = settings.getProperty ("mongodb.world.collection");
        final String FORTUNE = settings.getProperty ("mongodb.fortune.collection");

        MongoClient mongoClient = new MongoClient (HOST, PORT);
        MongoDatabase db = mongoClient.getDatabase (DATABASE);
        worldCollection = db.getCollection (WORLD);
        fortuneCollection = db.getCollection (FORTUNE);
    }

    @Override public List<Fortune> getFortunes () {
        List<Fortune> fortunes = new ArrayList<> ();

        fortuneCollection.find ().forEach ((Block<Document>)doc ->
            fortunes.add (new Fortune (doc.get ("_id", Double.class).intValue (), (String)doc.get
                ("message")))
        );

        return fortunes;
    }

    @Override public World[] getWorlds (int queries, boolean update) {
        final World[] worlds = new World[queries];
        final Random random = ThreadLocalRandom.current ();

        for (int ii = 0; ii < queries; ii++) {
            int id = random.nextInt (DB_ROWS) + 1;
            worlds[ii] = update? updateWorld (id, random.nextInt (DB_ROWS) + 1) : findWorld (id);
        }

        return worlds;
    }

    private World findWorld (int id) {
        return createWorld (worldCollection.find(eq ("_id", (double)id)).first ());
    }

    private World createWorld (Document world) {
        try {
            return new World (world.get ("_id", Double.class).intValue (), world.get
                ("randomNumber", Double.class).intValue ());
        }
        catch (ClassCastException e) {
            return new World (world.get ("_id", Double.class).intValue (), world.get
                ("randomNumber", Integer.class));
        }
    }

    public World updateWorld (int id, int random) {
        Document newWorld = new Document ("_id", (double)id).append ("randomNumber", (double)
            random);
        worldCollection.replaceOne (eq ("_id", (double)id), newWorld);

        return new World (id, random);
    }
}
