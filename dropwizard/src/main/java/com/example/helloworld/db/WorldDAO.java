
package com.example.helloworld.db;

import io.dropwizard.hibernate.AbstractDAO;
import org.hibernate.SessionFactory;

import com.example.helloworld.core.World;
import com.google.common.base.Optional;

public class WorldDAO
    extends AbstractDAO<World>
{
  public WorldDAO(SessionFactory factory)
  {
    super(factory);
  }

  public Optional<World> findById(Long id)
  {
    return Optional.fromNullable(get(id));
  }
}
