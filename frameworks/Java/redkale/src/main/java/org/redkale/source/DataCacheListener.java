/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.io.Serializable;

/**
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public interface DataCacheListener {

    public <T> int insertCache(Class<T> clazz, T... entitys);

    public <T> int updateCache(Class<T> clazz, T... entitys);

    public <T> int deleteCache(Class<T> clazz, Serializable... ids);
}
