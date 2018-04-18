/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.boot.watch;

import javax.annotation.Resource;
import org.redkale.boot.Application;
import org.redkale.net.TransportFactory;
import org.redkale.net.http.*;

/**
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@RestService(name = "service", catalog = "watch", repair = false)
public class ServiceWatchService extends AbstractWatchService {

    @Resource
    private Application application;

    @Resource
    private TransportFactory transportFactory;

//    @RestMapping(name = "load", auth = false, comment = "动态增加Service")
//    public RetResult loadService(String type, @RestUploadFile(maxLength = 10 * 1024 * 1024, fileNameReg = "\\.jar$") byte[] jar) {
//        //待开发
//        return RetResult.success();
//    }
//
//    @RestMapping(name = "stop", auth = false, comment = "动态停止Service")
//    public RetResult stopService(String name, String type) {
//        //待开发
//        return RetResult.success();
//    }
}
