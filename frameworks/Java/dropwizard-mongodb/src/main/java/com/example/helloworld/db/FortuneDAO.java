package com.example.helloworld.db;

import com.example.helloworld.db.model.Fortune;
import com.mongodb.DB;
import org.mongojack.JacksonDBCollection;

import java.util.Collection;

public class FortuneDAO {


    private final JacksonDBCollection<Fortune, String> fortunes;
    public FortuneDAO(DB db) {
        fortunes = JacksonDBCollection.wrap(db.getCollection("fortunes"), Fortune.class, String.class);
    }

    public Collection<? extends Fortune> list() {
        return null;
    }
}
