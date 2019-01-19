package com.example.helloworld.db.jdbi;

import java.util.List;

import org.skife.jdbi.v2.sqlobject.SqlQuery;
import org.skife.jdbi.v2.sqlobject.customizers.RegisterMapper;
import org.skife.jdbi.v2.sqlobject.mixins.Transactional;

import com.example.helloworld.db.FortuneDAO;
import com.example.helloworld.db.mappers.FortuneMapper;
import com.example.helloworld.db.model.Fortune;

public abstract class FortuneJDBIImpl implements Transactional<FortuneJDBIImpl>, FortuneDAO {
	@SqlQuery("select id, message from fortune")
	@RegisterMapper(FortuneMapper.class)
	public abstract List<Fortune> list();
}
