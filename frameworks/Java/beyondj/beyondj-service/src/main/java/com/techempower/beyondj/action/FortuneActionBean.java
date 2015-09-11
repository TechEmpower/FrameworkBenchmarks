package com.techempower.beyondj.action;

import com.techempower.beyondj.domain.Fortune;
import com.techempower.beyondj.repository.FortuneRepository;
import net.sourceforge.stripes.action.*;
import net.sourceforge.stripes.integration.spring.SpringBean;

import java.util.Collections;
import java.util.List;

@UrlBinding("/perf/fortunes/{_eventName}")
public class FortuneActionBean extends BaseActionBean {

    List<Fortune> fortunes;

    @SpringBean
    private FortuneRepository fortuneRepository;

    @DefaultHandler
    @HandlesEvent("fortunes")
    public Resolution fortunes() {
        fortunes = this.fortuneRepository.findAll();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        setResponseDate();
        return new ForwardResolution("/WEB-INF/templates/fortunes.jsp");
    }

    public List<Fortune> getFortunes() {
        return fortunes;
    }
}
