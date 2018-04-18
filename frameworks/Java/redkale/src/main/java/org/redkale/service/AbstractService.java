/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.service;

import java.util.concurrent.*;
import javax.annotation.Resource;
import org.redkale.net.*;

/**
 *
 * @author zhangjx
 */
public abstract class AbstractService implements Service {

    //如果开启了SNCP，此处线程池为SncpServer的线程池
    @Resource(name = Server.RESNAME_SERVER_EXECUTOR)
    private ExecutorService serverExecutor;

    protected void runAsync(Runnable runner) {
        if (serverExecutor != null) {
            serverExecutor.execute(runner);
        } else {
            Thread thread = Thread.currentThread();
            if (thread instanceof WorkThread) {
                ((WorkThread) thread).runAsync(runner);
            } else {
                ForkJoinPool.commonPool().execute(runner);
            }
        }
    }

    protected ExecutorService getExecutor() {
        if (serverExecutor != null) return serverExecutor;
        Thread thread = Thread.currentThread();
        if (thread instanceof WorkThread) {
            return ((WorkThread) thread).getExecutor();
        }
        return ForkJoinPool.commonPool();
    }
}
