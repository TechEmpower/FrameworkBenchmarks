/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.Type;

/**
 * 反序列化操作类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类
 * @param <T> 反解析的数据类型
 */
public interface Decodeable<R extends Reader, T> {

    public T convertFrom(final R in);

    /**
     * 泛型映射接口
     *
     * @return 反解析的数据类型
     */
    public Type getType();

}
