/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * 简易类的序列化和反序列化操作类   <br>
 * 能序列化为Boolean、Number或者字符串的类视为简易类  <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类
 * @param <W> Writer输出的子类
 * @param <T> 序列化/反解析的数据类型
 */
public abstract class SimpledCoder<R extends Reader, W extends Writer, T> implements Decodeable<R, T>, Encodeable<W, T> {

    private Type type;

    @Override
    public abstract void convertTo(final W out, final T value);

    @Override
    public abstract T convertFrom(final R in);

    @Override
    @SuppressWarnings("unchecked")
    public Class<T> getType() {
        if (type == null) {
            Type[] ts = ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments();
            type = ts[ts.length - 1];
        }
        return (Class<T>) type;
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
