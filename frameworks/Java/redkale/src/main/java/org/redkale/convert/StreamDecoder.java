/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import org.redkale.util.Creator;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;
import java.util.stream.Stream;

/**
 * Stream的反序列化操作类  <br>
 * 支持一定程度的泛型。  <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> 反解析的集合元素类型
 */
@SuppressWarnings("unchecked")
public final class StreamDecoder<T> implements Decodeable<Reader, Stream<T>> {

    private final Type type;

    private final Type componentType;

    protected Creator<Stream<T>> creator;

    protected final Decodeable<Reader, T> decoder;

    private boolean inited = false;

    private final Object lock = new Object();

    public StreamDecoder(final ConvertFactory factory, final Type type) {
        this.type = type;
        try {
            if (type instanceof ParameterizedType) {
                final ParameterizedType pt = (ParameterizedType) type;
                this.componentType = pt.getActualTypeArguments()[0];
                this.creator = factory.loadCreator((Class) pt.getRawType());
                factory.register(type, this);
                this.decoder = factory.loadDecoder(this.componentType);
            } else {
                throw new ConvertException("StreamDecoder not support the type (" + type + ")");
            }
        } finally {
            inited = true;
            synchronized (lock) {
                lock.notifyAll();
            }
        }
    }

    @Override
    public Stream<T> convertFrom(Reader in) {
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
        return result.stream();
    }

    @Override
    public Type getType() {
        return type;
    }

}
