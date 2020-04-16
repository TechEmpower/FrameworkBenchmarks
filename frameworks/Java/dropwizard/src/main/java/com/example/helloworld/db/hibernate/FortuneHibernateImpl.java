package com.example.helloworld.db.hibernate;

import java.util.List;

import org.hibernate.SessionFactory;
import org.hibernate.query.Query;

import com.example.helloworld.db.FortuneDAO;
import com.example.helloworld.db.model.Fortune;

import io.dropwizard.hibernate.AbstractDAO;

public class FortuneHibernateImpl extends AbstractDAO<Fortune> implements FortuneDAO {

	public FortuneHibernateImpl(SessionFactory factory) {
		super(factory);
	}

	@Override
	public List<Fortune> list() {
		return list((Query<Fortune>) query("SELECT f FROM Fortune f"));
	}
}
