package com.example.helloworld.db;

import com.example.helloworld.db.model.Fortune;
import io.dropwizard.hibernate.AbstractDAO;
import org.hibernate.SessionFactory;

import java.util.List;

public class FortuneDAO extends AbstractDAO<Fortune> {

    public FortuneDAO(SessionFactory factory) {
        super(factory);
    }

    public List<Fortune> list() {
        return list(criteria());
    }
}
