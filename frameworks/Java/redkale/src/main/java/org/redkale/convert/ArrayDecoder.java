/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.*;
import java.util.*;

/**
 * 数组的反序列化操作类  <br>
 * 对象数组的反序列化，不包含int[]、long[]这样的primitive class数组。  <br>
 * 支持一定程度的泛型。  <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> 反解析的数组元素类型
 */
@SuppressWarnings("unchecked")
public final class ArrayDecoder<T> implements Decodeable<Reader, T[]> {

    private final Type type;

    private final Type componentType;

    private final Class componentClass;

    protected final Decodeable<Reader, T> decoder;

    private boolean inited = false;

    private final Object lock = new Object();

    public ArrayDecoder(final ConvertFactory factory, final Type type) {
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
            if (this.componentType instanceof ParameterizedType) {
                this.componentClass = (Class) ((ParameterizedType) this.componentType).getRawType();
            } else {
                this.componentClass = (Class) this.componentType;
            }
            factory.register(type, this);
            this.decoder = factory.loadDecoder(this.componentType);
        } finally {
            inited = true;
            synchronized (lock) {
                lock.notifyAll();
            }
        }
    }

    @Override
    public T[] convertFrom(Reader in) {
        final int len = in.readArrayB();
        if (len == Reader.SIGN_NULL) return null;
        if (this.decoder == null) {
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
        final Decodeable<Reader, T> localdecoder = this.decoder;
        final List<T> result = new ArrayList();
        if (len == Reader.SIGN_NOLENGTH) {
            while (in.hasNext()) {
                result.add(localdecoder.convertFrom(in));
            }
        } else {
            for (int i = 0; i < len; i++) {
                result.add(localdecoder.convertFrom(in));
            }
        }
        in.readArrayE();
        T[] rs = (T[]) Array.newInstance((Class) this.componentClass, result.size());
        return result.toArray(rs);
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName() + "{componentType:" + this.componentType + ", decoder:" + this.decoder + "}";
    }

    @Override
    public Type getType() {
        return type;
    }

}
