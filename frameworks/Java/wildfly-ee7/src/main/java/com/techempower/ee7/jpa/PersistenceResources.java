package com.techempower.ee7.jpa;

import javax.enterprise.context.Dependent;
import javax.enterprise.inject.Produces;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

public class PersistenceResources {

    @PersistenceContext
    private EntityManager em;

    @Produces
    @Dependent
    public EntityManager entityManager() {
        return em;
    }
}
