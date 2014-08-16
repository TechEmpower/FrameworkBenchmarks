package com.techempower.ee7.jpa;

import javax.enterprise.context.RequestScoped;
import javax.enterprise.inject.Disposes;
import javax.enterprise.inject.Produces;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.PersistenceUnit;
import javax.persistence.SynchronizationType;

public class PersistenceResources {

  @PersistenceUnit
  private EntityManagerFactory entityManagerFactory;

  @Produces
  @RequestScoped
  public EntityManager entityManager() {
    return entityManagerFactory.createEntityManager(SynchronizationType.UNSYNCHRONIZED);
  }

  protected void closeEntityManager(@Disposes EntityManager entityManager) {
    if (entityManager.isOpen()) {
      entityManager.close();
    }
  }
}
