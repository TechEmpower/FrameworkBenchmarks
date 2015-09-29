package com.techempower.beyondj.action;

import net.sourceforge.stripes.action.DefaultHandler;
import net.sourceforge.stripes.action.ForwardResolution;
import net.sourceforge.stripes.action.Resolution;
import net.sourceforge.stripes.action.UrlBinding;

import java.util.HashMap;

/**
 * @author nickk
 */
@UrlBinding("/perf/error")
public class ErrorActionBean extends BaseActionBean {

    public static final String JSP = "/WEB-INF/templates/error.jsp";

    @DefaultHandler
    public Resolution view() {
        setResponseHeaders(new HashMap<>());
        return new ForwardResolution(JSP);
    }

    @Override
    protected Resolution getHomePage() {
        return null;
    }
}

