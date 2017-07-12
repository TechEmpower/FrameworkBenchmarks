package com.example.helloworld.db.mappers;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.skife.jdbi.v2.StatementContext;
import org.skife.jdbi.v2.tweak.ResultSetMapper;

import com.example.helloworld.db.model.Fortune;

public class FortuneMapper implements ResultSetMapper<Fortune> {
	public Fortune map(int index, ResultSet r, StatementContext ctx)
			throws SQLException {
		return new Fortune(r.getInt("id"), r.getString("message"));
	}
}