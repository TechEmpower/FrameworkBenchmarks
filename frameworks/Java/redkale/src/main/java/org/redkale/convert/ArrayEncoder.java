/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.*;

/**
 * 数组的序列化操作类  <br>
 * 对象数组的序列化，不包含int[]、long[]这样的primitive class数组。  <br>
 * 支持一定程度的泛型。  <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> 序列化的数组元素类型
 */
@SuppressWarnings("unchecked")
public final class ArrayEncoder<T> implements Encodeable<Writer, T[]> {

    private final Type type;

    private final Type componentType;

    private final Encodeable anyEncoder;

    private final Encodeable<Writer, Object> encoder;

    private boolean inited = false;

    private final Object lock = new Object();

    public ArrayEncoder(final ConvertFactory factory, final Type type) {
        this.type = type;
        try {
            if (type instanceof GenericArrayType) {
                Type t = ((GenericArrayType) type).getGenericComponentType();
                this.componentType = t instanceof TypeVariable ? Object.class : t;
            } else if ((type instanceof Class) && ((Class) type).isArray()) {
                this.componentType = ((Class) type).getComponentType();
            } else {
                throw new ConvertException("(" + type + ") is not a array type");
            }
            factory.register(type, this);
            this.encoder = factory.loadEncoder(this.componentType);
            this.anyEncoder = factory.getAnyEncoder();
        } finally {
            inited = true;
            synchronized (lock) {
                lock.notifyAll();
            }
        }
    }

    @Override
    public void convertTo(Writer out, T[] value) {
        if (value == null) {
            out.writeNull();
            return;
        }
        if (value.length == 0) {
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
        out.writeArrayB(value.length);
        final Type comp = this.componentType;
        boolean first = true;
        for (Object v : value) {
            if (!first) out.writeArrayMark();
            ((v != null && v.getClass() == comp) ? encoder : anyEncoder).convertTo(out, v);
            if (first) first = false;
        }
        out.writeArrayE();
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName() + "{componentType:" + this.componentType + ", encoder:" + this.encoder + "}";
    }

    @Override
    public Type getType() {
        return type;
    }
}
