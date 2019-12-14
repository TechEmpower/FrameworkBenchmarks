package com.example.helloworld.db.hibernate;

import io.dropwizard.hibernate.AbstractDAO;

import java.util.List;

import org.hibernate.SessionFactory;
import org.hibernate.query.Query;

import com.example.helloworld.db.FortuneDAO;
import com.example.helloworld.db.model.Fortune;

public class FortuneHibernateImpl extends AbstractDAO<Fortune> implements FortuneDAO {

    public FortuneHibernateImpl(SessionFactory factory) {
        super(factory);
    }

    public List<Fortune> list() {
        return list((Query<Fortune>) query("SELECT f FROM Fortune f"));
    }
}
