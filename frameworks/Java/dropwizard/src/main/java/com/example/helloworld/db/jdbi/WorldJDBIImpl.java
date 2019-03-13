package com.example.helloworld.db.jdbi;

import org.jdbi.v3.sqlobject.config.RegisterRowMapper;

import com.example.helloworld.db.mappers.WorldMapper;
import com.example.helloworld.db.model.World;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.SqlBatch;
import org.jdbi.v3.sqlobject.statement.SqlQuery;

@RegisterRowMapper(WorldMapper.class)
public interface WorldJDBIImpl {
	@SqlQuery("select id, randomNumber from world where id = :id")
	World findById(@Bind("id") int id);

	@SqlBatch("update world set randomNumber = :p.randomNumber where id = :p.id")
	void update(@BindBean("p") World...worlds);
}