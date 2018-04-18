/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.boot;

import java.lang.annotation.Annotation;
import org.redkale.net.*;
import org.redkale.net.http.*;
import org.redkale.service.Service;
import org.redkale.util.AnyValue;
import org.redkale.watch.*;

/**
 *
 * @author zhangjx
 */
@NodeProtocol({"WATCH"})
public class NodeWatchServer extends NodeHttpServer {

    public NodeWatchServer(Application application, AnyValue serconf) {
        super(application, serconf);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected ClassFilter<Service> createServiceClassFilter() {
        return createClassFilter(this.sncpGroup, null, WatchService.class, null, Annotation.class, "services", "service");
    }

    @Override
    @SuppressWarnings("unchecked")
    protected ClassFilter<Filter> createFilterClassFilter() {
        return createClassFilter(null, null, WatchFilter.class, null, null, "filters", "filter");
    }

    @Override
    @SuppressWarnings("unchecked")
    protected ClassFilter<Servlet> createServletClassFilter() {
        return createClassFilter(null, WebServlet.class, WatchServlet.class, null, null, "servlets", "servlet");
    }

    @Override
    protected ClassFilter createOtherClassFilter() {
        return null;
    }

    @Override
    public boolean isWATCH() {
        return true;
    }
}
