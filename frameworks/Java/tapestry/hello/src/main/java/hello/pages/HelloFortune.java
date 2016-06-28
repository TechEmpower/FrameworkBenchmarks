package hello.pages;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.tapestry5.annotations.Property;
import org.apache.tapestry5.ioc.annotations.Inject;
import org.apache.tapestry5.services.Request;

import hello.entities.Fortune;

/**
 * Test type 4: Fortunes
 */
public class HelloFortune {
  @Inject
  private org.hibernate.Session session;

  @Inject
  private Request request;

  @Property
  private Fortune currentFortune;

  public List getFortunes() {
    final List<Fortune> fortunesFromDB = session.createCriteria(Fortune.class).list();
    final List<Fortune> fortunes = new ArrayList<>(fortunesFromDB.size() + 1);
    fortunes.addAll(fortunesFromDB);
    Fortune additionalFortune = new Fortune();
    additionalFortune.message = "Additional fortune added at request time.";
    fortunes.add(additionalFortune);
    Collections.sort(fortunes);
    return fortunes;
  }

}
