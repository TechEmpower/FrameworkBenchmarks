package com.example.helloworld.db.mappers;

import java.sql.ResultSet;
import java.sql.SQLException;

import com.example.helloworld.db.model.World;
import org.jdbi.v3.core.mapper.RowMapper;
import org.jdbi.v3.core.statement.StatementContext;

public class WorldMapper implements RowMapper<World> {
	@Override
	public World map(ResultSet rs, StatementContext ctx) throws SQLException {
		return new World(rs.getInt("id"), rs.getInt("randomNumber"));
	}
}