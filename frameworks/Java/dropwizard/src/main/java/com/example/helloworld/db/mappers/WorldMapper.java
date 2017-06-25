package com.example.helloworld.db.mappers;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.skife.jdbi.v2.StatementContext;
import org.skife.jdbi.v2.tweak.ResultSetMapper;

import com.example.helloworld.db.model.World;

public class WorldMapper implements ResultSetMapper<World> {
	public World map(int index, ResultSet r, StatementContext ctx)
			throws SQLException {
		return new World(r.getInt("id"), r.getInt("randomNumber"));
	}
}