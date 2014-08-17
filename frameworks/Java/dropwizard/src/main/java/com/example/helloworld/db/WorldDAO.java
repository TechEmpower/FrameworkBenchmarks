package com.example.helloworld.db;

import com.example.helloworld.db.model.World;
import com.google.common.base.Optional;
import io.dropwizard.hibernate.AbstractDAO;
import org.hibernate.SessionFactory;

public class WorldDAO extends AbstractDAO<World> {

    public WorldDAO(SessionFactory factory) {
        super(factory);
    }

    public Optional<World> findById(Long id) {
        return Optional.fromNullable(get(id));
    }

    public World update(World world) {
        return persist(world);
    }
}
