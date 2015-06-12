package com.example.helloworld.db;

import com.example.helloworld.db.model.World;
import com.google.common.base.Optional;
import com.mongodb.DB;
import org.mongojack.JacksonDBCollection;

public class WorldDAO {

    private final JacksonDBCollection<World, String> worlds;

    public WorldDAO(DB db) {
        worlds = JacksonDBCollection.wrap(db.getCollection("world"), World.class, String.class);
    }

    public Optional<World> findById(long worldId) {
        return Optional.fromNullable(worlds.findOneById(String.valueOf(worldId)));
    }

    public World update(World world) {
        return worlds.insert(world).getSavedObject();
    }
}
