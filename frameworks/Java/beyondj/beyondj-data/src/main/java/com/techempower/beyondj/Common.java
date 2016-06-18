package com.techempower.beyondj;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * User: Denis Baranov
 * Date: 2/18/15
 */
public class Common {

    public static final int CPU_COUNT = Runtime.getRuntime().availableProcessors();
    public static int CORE_POOL_SIZE;
    public static int MAX_POOL_SIZE;
    public static int keepAliveTime = 200;


    static {
        CORE_POOL_SIZE = CPU_COUNT * 2;
        MAX_POOL_SIZE = CPU_COUNT * 25;
    }

    // todo: parameterize multipliers
    public static ExecutorService EXECUTOR = new ThreadPoolExecutor(
            CORE_POOL_SIZE, MAX_POOL_SIZE, keepAliveTime, TimeUnit.MILLISECONDS,
            new LinkedBlockingQueue<Runnable>(CPU_COUNT * 100),
            new ThreadPoolExecutor.CallerRunsPolicy());
}
