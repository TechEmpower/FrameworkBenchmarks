package com.techempower.beyondj.action;

import com.techempower.beyondj.BeyondJActionBeanContext;
import net.sourceforge.stripes.action.*;
import net.sourceforge.stripes.validation.ValidationErrorHandler;
import net.sourceforge.stripes.validation.ValidationErrors;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author nickk
 */
public abstract class BaseActionBean implements
        ValidationErrorHandler, ActionBean {

    protected static final String WEB_HOME_VIEW = "/perf/hello";
    protected static final String ERROR_JSP = "/WEB-INF/templates/error.jsp";
    public static final String DATE = "Date";

    protected BeyondJActionBeanContext context;

    public ActionBeanContext getContext() {
        return context;
    }

    public String getSessionId() {
        return getContext().getRequest().getSession().getId();
    }

    public void setContext(ActionBeanContext context) {
        this.context = (BeyondJActionBeanContext) context;
    }

    public Map<String, String> getNormalizedRequestParameters() {

        Map<String, String[]> requestParameters = context.getRequest()
                .getParameterMap();
        Map<String, String> map = new HashMap<String, String>();

        for (Map.Entry<String, String[]> entry : requestParameters.entrySet()) {
            String name = entry.getKey().trim();
            String[] values = getContext().getRequest()
                    .getParameterValues(name);

            for (String val : values) {
                map.put(name, val);
            }
        }
        return map;
    }

    public String getContextPath() {
        return getContext().getRequest().getContextPath();
    }

    public Resolution handleValidationErrors(ValidationErrors errors)
            throws Exception {
        if (errors.size() > 0) {
            System.out.println("" + getNormalizedRequestParameters());
            System.out.println("\n\nPlease override handleValidationErrors() in your current action bean");
            System.out.println("To see the on the current page, please override getErrorPage() with the name of the current page and remember to call this tag <stripes:errors/>");
            Set<String> keys = errors.keySet();
            for (String key : keys) {
                System.out.println("Validation Error Key : "
                        + errors.get(key).get(0).getFieldName());
                System.out.println("Params are : " + getNormalizedRequestParameters());
            }
        }
        return new ForwardResolution(getErrorPage());
    }

    protected String getErrorPage() {
        return ERROR_JSP;
    }

    protected Resolution getHomePage() {
        return new RedirectResolution(WEB_HOME_VIEW);
    }

    public String getImagePath() {
        return getContextPath() + "/images";
    }

    public String getImageFolder() {
        return "images";
    }

    public String getCssPath() {
        return getContextPath() + "/css";
    }

    public String getScriptPath() {
        return getContextPath() + "/js";
    }

    public String getTarget() {
        return getContext().getRequest().getPathInfo();
    }

    protected void setResponseHeaders(Map<String,String>headers){
        for(String key: headers.keySet()){
            getContext().getResponse().setHeader(key, headers.get(key));
        }
        getContext().getResponse().setHeader(DATE, new java.util.Date().toString());
    }
}

