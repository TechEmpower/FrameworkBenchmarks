package com.wizzardo.techempower;

import com.wizzardo.http.framework.di.PostConstruct;
import com.wizzardo.http.framework.di.Service;
import com.wizzardo.tools.cache.Cache;
import io.reactiverse.pgclient.PgIterator;
import io.reactiverse.pgclient.PgPool;
import io.reactiverse.pgclient.Row;
import io.reactiverse.pgclient.Tuple;

public class CachedWorldService implements Service, PostConstruct {

    DBService dbService;
    Cache<Integer, CachedWorld> worldCache = new Cache<>(-1);


    @Override
    public void init() {

        PgPool pool = dbService.getClient();
        pool.preparedQuery("SELECT * FROM World", res -> {
            if (res.succeeded()) {
                for (Row row : res.result()) {
                    CachedWorld cachedWorld = new CachedWorld(row.getInteger(0), row.getInteger(1));
                    worldCache.put(cachedWorld.id, cachedWorld);
                }
            } else {
                res.cause().printStackTrace();
            }
        });
    }

    public CachedWorld get(int id) {
        return worldCache.get(id);
    }
}
