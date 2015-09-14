package com.techempower.beyondj.action;

import com.techempower.beyondj.domain.Fortune;
import com.techempower.beyondj.repository.FortuneRepository;
import com.techempower.beyondj.repository.WorldRepository;
import net.sourceforge.stripes.action.*;
import net.sourceforge.stripes.integration.spring.SpringBean;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;

import javax.persistence.EntityManagerFactory;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

@UrlBinding("/perf/fortunes/{_eventName}")
public class FortuneActionBean extends BaseActionBean {

    private List<Fortune> fortunes;

    @DefaultHandler
    @HandlesEvent("fortunes")
    public Resolution fortunes() {
       // validateRepository();
        Iterable<Fortune> it = this.fortuneRepository.findAll();
        fortunes = new ArrayList<>();
        Iterator<Fortune> iterator = it.iterator();
        while (iterator.hasNext()) {
            fortunes.add(iterator.next());
        }
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        setResponseDate();
        return new ForwardResolution("/WEB-INF/templates/fortunes.jsp");
    }

    public List<Fortune> getFortunes() {
        return fortunes;
    }

   /* private void validateRepository() {
        if (fortuneRepository == null) {
            synchronized (FortuneActionBean.class) {
                fortuneRepository = new SimpleJpaRepository<>(
                        Fortune.class, entityManagerFactory.createEntityManager());
            }
        }
    }*/

    @SpringBean
    private EntityManagerFactory entityManagerFactory;
   // private static SimpleJpaRepository fortuneRepository;

    @SpringBean
    private FortuneRepository fortuneRepository;
}
