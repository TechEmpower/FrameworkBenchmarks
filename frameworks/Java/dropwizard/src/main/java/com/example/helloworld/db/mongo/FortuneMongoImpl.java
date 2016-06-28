package com.example.helloworld.db.mongo;

import com.example.helloworld.db.FortuneDAO;
import com.example.helloworld.db.model.Fortune;
import org.mongojack.DBProjection;
import org.mongojack.DBQuery;
import org.mongojack.JacksonDBCollection;

import java.util.List;

public class FortuneMongoImpl implements FortuneDAO {

    private final JacksonDBCollection<Fortune, Integer> fortuneCollection;

    public FortuneMongoImpl(JacksonDBCollection<Fortune, Integer> fortuneCollection) {
        this.fortuneCollection = fortuneCollection;
    }
    @Override
    public List<Fortune> list() {
        return fortuneCollection.find(DBQuery.empty(), DBProjection.include("_id", "message")).toArray();
    }
}
