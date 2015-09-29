package com.techempower.beyondj.action;

import com.techempower.beyondj.ApplicationContextProvider;
import com.techempower.beyondj.domain.Fortune;
import com.techempower.beyondj.repository.FortuneRepository;
import com.techempower.beyondj.repository.WorldRepository;
import net.sourceforge.stripes.action.*;
import net.sourceforge.stripes.integration.spring.SpringBean;
import net.sourceforge.stripes.validation.ValidationMethod;
import net.sourceforge.stripes.validation.ValidationState;

import java.util.*;

@UrlBinding("/perf/fortunes/{_eventName}")
public class FortuneActionBean extends BaseActionBean {

    private List<Fortune> fortunes;

    @DefaultHandler
    @HandlesEvent(FORTUNES)
    public Resolution fortunes() {
        Iterable<Fortune> it = this.fortuneRepository.findAll();
        fortunes = new ArrayList<>();
        Iterator<Fortune> iterator = it.iterator();
        while (iterator.hasNext()) {
            fortunes.add(iterator.next());
        }
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        Map<String, String> headers = new HashMap<>();
        headers.put(TRANSFER_ENCODING, Boolean.TRUE.toString());
        setResponseHeaders(headers);
        return new ForwardResolution(JSP);
    }

    public List<Fortune> getFortunes() {
        return fortunes;
    }

    @ValidationMethod(when = ValidationState.ALWAYS)
    public void validateRepository() {
        fortuneRepository = (FortuneRepository) applicationContextProvider.getApplicationContext().getBean(FORTUNE_REPOSITORY);
    }

    private FortuneRepository fortuneRepository;

    @SpringBean
    private ApplicationContextProvider applicationContextProvider;

    public static final String FORTUNES = "fortunes";
    public static final String JSP = "/WEB-INF/templates/fortunes.jsp";
    public static final String TRANSFER_ENCODING = "Transfer-Encoding";
    public static final String FORTUNE_REPOSITORY = "fortuneRepository";
}

