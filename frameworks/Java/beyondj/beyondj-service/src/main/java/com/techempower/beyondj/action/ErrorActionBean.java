package com.techempower.beyondj.action;

import net.sourceforge.stripes.action.DefaultHandler;
import net.sourceforge.stripes.action.ForwardResolution;
import net.sourceforge.stripes.action.Resolution;
import net.sourceforge.stripes.action.UrlBinding;

/**
 * @author nickk
 */
@UrlBinding("/perf/error")
public class ErrorActionBean extends BaseActionBean {

    @DefaultHandler
    public Resolution view() {
        setResponseDate();
        return new ForwardResolution("/WEB-INF/templates/error.jsp");
    }

    @Override
    protected Resolution getHomePage() {
        return null;
    }
}
