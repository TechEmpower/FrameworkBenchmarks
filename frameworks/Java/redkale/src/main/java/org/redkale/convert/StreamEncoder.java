/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.*;
import java.util.stream.Stream;

/**
 * Stream的序列化操作类  <br>
 * 支持一定程度的泛型。  <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> 序列化的集合元素类型
 */
@SuppressWarnings("unchecked")
public final class StreamEncoder<T> implements Encodeable<Writer, Stream<T>> {

    private final Type type;

    private final Encodeable<Writer, Object> encoder;

    private boolean inited = false;

    private final Object lock = new Object();

    public StreamEncoder(final ConvertFactory factory, final Type type) {
        this.type = type;
        try {
            if (type instanceof ParameterizedType) {
                Type t = ((ParameterizedType) type).getActualTypeArguments()[0];
                if (t instanceof TypeVariable) {
                    this.encoder = factory.getAnyEncoder();
                } else {
                    this.encoder = factory.loadEncoder(t);
                }
            } else {
                this.encoder = factory.getAnyEncoder();
            }
        } finally {
            inited = true;
            synchronized (lock) {
                lock.notifyAll();
            }
        }
    }

    @Override
    public void convertTo(Writer out, Stream<T> value) {
        if (value == null) {
            out.writeNull();
            return;
        }
        Object[] array = value.toArray();
        if (array.length == 0) {
            out.writeArrayB(0);
            out.writeArrayE();
            return;
        }
        if (this.encoder == null) {
            if (!this.inited) {
                synchronized (lock) {
                    try {
                        lock.wait();
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        out.writeArrayB(array.length);
        boolean first = true;
        for (Object v : array) {
            if (!first) out.writeArrayMark();
            encoder.convertTo(out, v);
            if (first) first = false;
        }
        out.writeArrayE();
    }

    @Override
    public Type getType() {
        return type;
    }
}
