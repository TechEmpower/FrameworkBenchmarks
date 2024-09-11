package com.techempower.ee7.jpa;

import jakarta.enterprise.context.Dependent;
import jakarta.enterprise.inject.Produces;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;

public class PersistenceResources {

    @PersistenceContext
    private EntityManager em;

    @Produces
    @Dependent
    public EntityManager entityManager() {
        return em;
    }
}
