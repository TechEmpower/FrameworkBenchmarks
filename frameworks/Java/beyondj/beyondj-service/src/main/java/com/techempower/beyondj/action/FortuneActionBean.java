package com.techempower.beyondj.action;

import com.techempower.beyondj.domain.Fortune;
import com.techempower.beyondj.repository.FortuneRepository;
import net.sourceforge.stripes.action.*;
import net.sourceforge.stripes.integration.spring.SpringBean;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@UrlBinding("/perf/fortunes/{_eventName}")
public class FortuneActionBean extends BaseActionBean {

    private List<Fortune> fortunes;

    @DefaultHandler
    @HandlesEvent(FORTUNES)
    public Resolution fortunes() throws Exception{
        fortunes = this.fortuneRepository.findAll();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        Map<String, String> headers = new HashMap<>();
        headers.put(TRANSFER_ENCODING, Boolean.TRUE.toString());
        getContext().getResponse().setCharacterEncoding("UTF-8");
        getContext().getRequest().setCharacterEncoding("UTF-8");
        getContext().getResponse().setContentType("text/html");
        setResponseHeaders(headers);
        return new ForwardResolution(JSP);
    }

    public List<Fortune> getFortunes() {
        return fortunes;
    }

    @SpringBean
    private FortuneRepository fortuneRepository;

    public static final String FORTUNES = "fortunes";
    public static final String JSP = "/WEB-INF/templates/fortunes.jsp";
    public static final String TRANSFER_ENCODING = "Transfer-Encoding";
}

