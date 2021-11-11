package com.example.helloworld.db.mappers;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.jdbi.v3.core.mapper.RowMapper;
import org.jdbi.v3.core.statement.StatementContext;

import com.example.helloworld.db.model.Fortune;

public class FortuneMapper implements RowMapper<Fortune> {
	@Override
	public Fortune map(ResultSet rs, StatementContext ctx) throws SQLException {
		return new Fortune(rs.getInt("id"), rs.getString("message"));
	}
}