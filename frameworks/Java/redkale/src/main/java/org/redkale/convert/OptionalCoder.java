/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.*;
import java.util.*;

/**
 * Optional 的SimpledCoder实现
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
public class OptionalCoder<R extends Reader, W extends Writer, T> extends SimpledCoder<R, W, Optional<T>> {

    private final Type type;

    private final Type componentType;

    protected final Class componentClass;

    protected final Decodeable<Reader, T> decoder;

    protected final Encodeable<Writer, T> encoder;

    private boolean inited = false;

    private final Object lock = new Object();

    @SuppressWarnings("unchecked")
    public OptionalCoder(final ConvertFactory factory, final Type type) {
        this.type = type;
        try {
            if (type instanceof ParameterizedType) {
                final ParameterizedType pt = (ParameterizedType) type;
                this.componentType = pt.getActualTypeArguments()[0];
                factory.register(type, this);
                this.decoder = factory.loadDecoder(this.componentType);
                if (this.componentType instanceof TypeVariable) {
                    this.encoder = factory.getAnyEncoder();
                    this.componentClass = Object.class;
                } else {
                    if (componentType instanceof ParameterizedType) {
                        final ParameterizedType pt2 = (ParameterizedType) componentType;
                        this.componentClass = (Class) pt2.getRawType();
                    } else {
                        this.componentClass = (Class) componentType;
                    }
                    this.encoder = factory.loadEncoder(this.componentType);
                }
            } else {
                this.componentType = Object.class;
                this.componentClass = Object.class;
                this.decoder = factory.loadDecoder(this.componentType);
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
    public void convertTo(W out, Optional<T> value) {
        if (value == null || !value.isPresent()) {
            out.writeObjectNull(null);
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
        this.encoder.convertTo(out, value.get());
    }

    @Override
    public Optional<T> convertFrom(R in) {
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
        return Optional.ofNullable(this.decoder.convertFrom(in));
    }

}
