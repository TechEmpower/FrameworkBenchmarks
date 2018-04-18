/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.boot;

/**
 * NodeServer的拦截类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class NodeInterceptor {

    /** *
     * Server.start之前调用 <br>
     * NodeServer.start的部署是先执行NodeInterceptor.preStart，再执行 Server.start 方法
     *
     * @param server NodeServer
     */
    public void preStart(NodeServer server) {

    }

    /**
     * Server.shutdown之前调用 <br>
     * NodeServer.shutdown的部署是先执行NodeInterceptor.preShutdown，再执行 Server.sshutdown 方法
     *
     * @param server NodeServer
     */
    public void preShutdown(NodeServer server) {

    }

}
