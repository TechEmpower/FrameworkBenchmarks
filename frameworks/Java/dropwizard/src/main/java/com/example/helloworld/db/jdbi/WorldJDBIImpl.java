package com.example.helloworld.db.jdbi;

import org.skife.jdbi.v2.sqlobject.Bind;
import org.skife.jdbi.v2.sqlobject.BindBean;
import org.skife.jdbi.v2.sqlobject.SqlQuery;
import org.skife.jdbi.v2.sqlobject.SqlUpdate;
import org.skife.jdbi.v2.sqlobject.customizers.RegisterMapper;
import org.skife.jdbi.v2.sqlobject.mixins.Transactional;

import com.example.helloworld.db.mappers.WorldMapper;
import com.example.helloworld.db.model.World;

@RegisterMapper(WorldMapper.class)
public abstract class WorldJDBIImpl implements Transactional<WorldJDBIImpl>, AutoCloseable  {
	@SqlQuery("select id, randomNumber from world where id = :id")
	public abstract World findById(@Bind("id") int id);

	@SqlUpdate("update world set randomNumber = :p.randomNumber where id = :p.id")
	public abstract long update(@BindBean("p") World world);
}