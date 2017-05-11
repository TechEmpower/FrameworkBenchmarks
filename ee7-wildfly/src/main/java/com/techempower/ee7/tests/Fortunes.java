package com.techempower.ee7.tests;

import java.util.Collections;
import java.util.List;

import javax.annotation.PostConstruct;
import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.inject.Named;
import javax.persistence.EntityManager;

import com.techempower.ee7.model.Fortune;

@RequestScoped
@Named
public class Fortunes {

  @Inject
  private EntityManager em;

  private List<Fortune> data;

  @PostConstruct
  private void postConstruct() {
    data = em.createQuery("SELECT f FROM Fortune f", Fortune.class).getResultList();
    data.add(new Fortune(0, "Additional fortune added at request time."));
    Collections.sort(data);
  }

  public List<Fortune> getData() {
    return data;
  }
}
