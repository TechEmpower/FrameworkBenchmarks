package com.techempower.beyondj.action;

import com.techempower.beyondj.domain.Fortune;
import com.techempower.beyondj.repository.FortuneRepository;
import net.sourceforge.stripes.action.*;
import net.sourceforge.stripes.integration.spring.SpringBean;

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

    @SpringBean
    private EntityManagerFactory entityManagerFactory;
    @SpringBean
    private FortuneRepository fortuneRepository;
}
