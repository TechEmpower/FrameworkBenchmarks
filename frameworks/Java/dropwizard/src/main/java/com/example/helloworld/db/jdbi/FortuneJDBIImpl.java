package com.example.helloworld.db.jdbi;

import java.util.List;

import com.example.helloworld.db.mappers.FortuneMapper;
import com.example.helloworld.db.model.Fortune;
import org.jdbi.v3.sqlobject.config.RegisterRowMapper;
import org.jdbi.v3.sqlobject.statement.SqlQuery;

public interface FortuneJDBIImpl {
	@SqlQuery("select id, message from fortune")
	@RegisterRowMapper(FortuneMapper.class)
	List<Fortune> list();
}
