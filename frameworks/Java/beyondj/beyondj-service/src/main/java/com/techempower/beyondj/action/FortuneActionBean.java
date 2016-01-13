package com.techempower.beyondj.action;

import com.techempower.beyondj.domain.Fortune;
import com.techempower.beyondj.repository.FortuneRepository;
import net.sourceforge.stripes.action.*;
import net.sourceforge.stripes.integration.spring.SpringBean;
import org.apache.commons.lang3.StringEscapeUtils;

import java.util.*;

@UrlBinding("/perf/fortunes/{_eventName}")
public class FortuneActionBean extends BaseActionBean {

    private List<Fortune> fortunes;

    @DefaultHandler
    @HandlesEvent(FORTUNES)
    public Resolution fortunes() {
/*
        Itterable<Fortune> it = this.fortuneRepository.findAll();
        fortunes = new ArrayList<>();
        Iterator<Fortune> iterator = it.iterator();
        while (iterator.hasNext()) {
            Fortune fortune = iterator.next();
            fortune.setMessage(StringEscapeUtils.escapeHtml4(fortune.getMessage()));
            fortune.setMessage(fortune.getMessage().replaceAll("<script>", "&ltscript&gt"));
            fortunes.add(fortune);
        }
*/
        fortunes = this.fortuneRepository.findAll();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        Map<String, String> headers = new HashMap<>();
        headers.put(TRANSFER_ENCODING, Boolean.TRUE.toString());
        getContext().getResponse().setCharacterEncoding(UTF_8);
        setResponseHeaders(headers);
        return new ForwardResolution(JSP);
    }

    public List<Fortune> getFortunes() {
        return fortunes;
    }

    @SpringBean
    private FortuneRepository fortuneRepository;

    public static final String UTF_8 = "UTF-8";
    public static final String FORTUNES = "fortunes";
    public static final String JSP = "/WEB-INF/templates/fortunes.jsp";
    public static final String TRANSFER_ENCODING = "Transfer-Encoding";
}

