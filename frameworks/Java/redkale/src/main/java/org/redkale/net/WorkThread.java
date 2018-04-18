/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net;

import java.util.concurrent.*;

/**
 * 协议处理的自定义线程类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class WorkThread extends Thread {

    private final ExecutorService executor;

    public WorkThread(ExecutorService executor, Runnable runner) {
        super(runner);
        this.executor = executor;
        this.setDaemon(true);
    }

    public void runAsync(Runnable runner) {
        executor.execute(runner);
    }

    public ExecutorService getExecutor() {
        return executor;
    }
}
