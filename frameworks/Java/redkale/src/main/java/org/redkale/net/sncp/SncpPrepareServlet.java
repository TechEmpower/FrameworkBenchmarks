/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.sncp;

import org.redkale.net.PrepareServlet;
import org.redkale.util.AnyValue;
import java.io.IOException;
import org.redkale.service.Service;
import org.redkale.util.*;

/**
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class SncpPrepareServlet extends PrepareServlet<DLong, SncpContext, SncpRequest, SncpResponse, SncpServlet> {

    private final Object sncplock = new Object();


    @Override
    public void addServlet(SncpServlet servlet, Object attachment, AnyValue conf, DLong... mappings) {
        synchronized (sncplock) {
            for (SncpServlet s : getServlets()) {
                if (s.service == servlet.service) throw new RuntimeException(s.service + " repeat addSncpServlet");
            }
            setServletConf(servlet, conf);
            putMapping(servlet.getServiceid(), servlet);
            putServlet(servlet);
        }
    }

    public <T> SncpServlet removeSncpServlet(Service service) {
        SncpServlet rs = null;
        synchronized (sncplock) {
            for (SncpServlet servlet : getServlets()) {
                if (servlet.service == service) {
                    rs = servlet;
                    break;
                }
            }
            if (rs != null) {
                removeMapping(rs);
                removeServlet(rs);
            }
        }
        return rs;
    }

    @Override
    public void init(SncpContext context, AnyValue config) {
        super.init(context, config); //必须要执行
        getServlets().forEach(s -> s.init(context, getServletConf(s)));
    }

    @Override
    public void destroy(SncpContext context, AnyValue config) {
        super.destroy(context, config); //必须要执行
        getServlets().forEach(s -> s.destroy(context, getServletConf(s)));
    }

    @Override
    public void execute(SncpRequest request, SncpResponse response) throws IOException {
        if (request.isPing()) {
            response.finish(Sncp.PONG_BUFFER.duplicate());
            return;
        }
        SncpServlet servlet = (SncpServlet) mappingServlet(request.getServiceid());
        if (servlet == null) {
            response.finish(SncpResponse.RETCODE_ILLSERVICEID, null);  //无效serviceid
        } else {
            servlet.execute(request, response);
        }
    }

}
