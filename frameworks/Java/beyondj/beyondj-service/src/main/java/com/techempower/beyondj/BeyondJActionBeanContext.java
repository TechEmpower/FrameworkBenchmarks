package com.techempower.beyondj;

import net.sourceforge.stripes.action.ActionBeanContext;
import net.sourceforge.stripes.action.ForwardResolution;
import net.sourceforge.stripes.action.Resolution;
import org.apache.commons.lang.StringUtils;

/**
 * @author nickk
 */
public class BeyondJActionBeanContext extends ActionBeanContext {

    @Override
    public Resolution getSourcePageResolution() {
        String sourcePage = getSourcePage();
        if (sourcePage == null) {
            return new ForwardResolution(getRequest().getServletPath());
        }
        return super.getSourcePageResolution();
    }

    public String getTarget() {
        String target = this.getRequest().getRequestURI();
        if (!StringUtils.isBlank(target)) {
            String appName = this.getRequest().getContextPath();
            int index = target.indexOf(appName);
            if (index != -1) {
                target = target.substring(index + appName.length());
            }
        }
        return target;
    }
}
