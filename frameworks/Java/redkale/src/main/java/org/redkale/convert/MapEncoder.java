/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Map;

/**
 * Map的序列化操作类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <K> Map key的数据类型
 * @param <V> Map value的数据类型
 */
@SuppressWarnings("unchecked")
public final class MapEncoder<K, V> implements Encodeable<Writer, Map<K, V>> {

    private final Type type;

    private final Encodeable<Writer, K> keyencoder;

    private final Encodeable<Writer, V> valencoder;

    private boolean inited = false;

    private final Object lock = new Object();

    public MapEncoder(final ConvertFactory factory, final Type type) {
        this.type = type;
        try {
            if (type instanceof ParameterizedType) {
                final Type[] pt = ((ParameterizedType) type).getActualTypeArguments();
                this.keyencoder = factory.loadEncoder(pt[0]);
                this.valencoder = factory.loadEncoder(pt[1]);
            } else {
                this.keyencoder = factory.getAnyEncoder();
                this.valencoder = factory.getAnyEncoder();
            }
        } finally {
            inited = true;
            synchronized (lock) {
                lock.notifyAll();
            }
        }
    }

    @Override
    public void convertTo(Writer out, Map<K, V> value) {
        final Map<K, V> values = value;
        if (values == null) {
            out.writeNull();
            return;
        }

        if (this.keyencoder == null || this.valencoder == null) {
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
        out.writeMapB(values.size());
        boolean first = true;
        for (Map.Entry<K, V> en : values.entrySet()) {
            if (!first) out.writeArrayMark();
            this.keyencoder.convertTo(out, en.getKey());
            out.writeMapMark();
            this.valencoder.convertTo(out, en.getValue());
            if (first) first = false;
        }
        out.writeMapE();
    }

    @Override
    public Type getType() {
        return type;
    }
}
