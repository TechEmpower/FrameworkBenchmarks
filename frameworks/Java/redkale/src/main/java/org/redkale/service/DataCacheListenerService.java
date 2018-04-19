/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.service;

import java.io.*;
import javax.annotation.*;
import org.redkale.source.*;
import org.redkale.util.*;

/**
 * 实现进程间DataSource的缓存数据同步
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@AutoLoad(false)
@ResourceType(DataCacheListener.class)
public class DataCacheListenerService implements DataCacheListener, Service {

    @Resource(name = "$")
    private DataSource source;

    @Override
    @RpcMultiRun(selfrun = false, async = true)
    public <T> int insertCache(Class<T> clazz, T... entitys) {
        if (!(source instanceof DataCacheListener)) return -2;
        return ((DataCacheListener) source).insertCache(clazz, entitys);
    }

    @Override
    @RpcMultiRun(selfrun = false, async = true)
    public <T> int updateCache(Class<T> clazz, T... entitys) {
        if (!(source instanceof DataCacheListener)) return -2;
        return ((DataCacheListener) source).updateCache(clazz, entitys);
    }

    @Override
    @RpcMultiRun(selfrun = false, async = true)
    public <T> int deleteCache(Class<T> clazz, Serializable... ids) {
        if (!(source instanceof DataCacheListener)) return -2;
        return ((DataCacheListener) source).deleteCache(clazz, ids);
    }

}
