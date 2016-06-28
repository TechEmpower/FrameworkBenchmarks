package com.example.helloworld.db.hibernate;

import com.example.helloworld.db.FortuneDAO;
import com.example.helloworld.db.model.Fortune;
import io.dropwizard.hibernate.AbstractDAO;
import org.hibernate.SessionFactory;

import java.util.List;

public class FortuneHibernateImpl extends AbstractDAO<Fortune> implements FortuneDAO {

    public FortuneHibernateImpl(SessionFactory factory) {
        super(factory);
    }

    public List<Fortune> list() {
        return list(criteria());
    }
}
